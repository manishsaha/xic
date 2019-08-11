open Ast
open Common
open Core
open Ir
open Mangle

let var_prefix = "$"
let make_var id ctx =
  match List.Assoc.find ctx id ~equal:(=) with
  | Some (`VarType _) -> `IRMem (`IRName id)
  | Some (`FuncType _) -> `IRName id
  | Some _ -> failwith "impossible"
  | None -> `IRTemp (var_prefix ^ id)

let ir_locations = ir_empty_loc_table ()
let locations = empty_loc_table ()

(* Reserved temp for "this" *)
let this_tmp = `IRTemp "_THIS"

(* Maps for translating objects *)
let class_map : (id, xi_class_type) Hash_table.t = Hash_table.create (module String)
let vt_map : (id, (id, int) Hash_table.t) Hash_table.t = Hash_table.create (module String)
let vt_size_map : (id, int) Hash_table.t = Hash_table.create (module String)
let layout_map : (id, (id, int) Hash_table.t) Hash_table.t = Hash_table.create (module String)

(* Helper functions to comply with the ABI *)
let size_of_class c_id = "_I_size_" ^ encode_id c_id
let init_of_class c_id = "_I_init_" ^ encode_id c_id
let vt_of_class c_id = "_I_vt_" ^ encode_id c_id

(* Translate binop codes to valid MIR *)
let mir_of_binop b =
  match b with
  | `Plus -> `IR_ADD
  | `Minus -> `IR_SUB
  | `Mult -> `IR_MUL
  | `HighMult -> `IR_HMUL
  | `Div -> `IR_DIV
  | `Mod -> `IR_MOD
  | `Eq -> `IR_EQ
  | `Neq -> `IR_NEQ
  | `Leq -> `IR_LEQ
  | `Lt -> `IR_LT
  | `Geq -> `IR_GEQ
  | `Gt -> `IR_GT
  | `And -> `IR_AND
  | `Or -> `IR_OR
  | `Dot
  | `Index
  | `Concat -> failwith "impossible"

(* Given a list of exprs [es], convert to MIR equivalent *)
let mir_add es =
  match es with
  | [] -> failwith "cannot add with no arguments"
  | e :: es ->
    List.fold es ~init:e ~f:(fun acc e -> `IRBinary (`IR_ADD, acc, e))

(* MIR representation of offset 8*i *)
let mir_offset e = `IRBinary (`IR_MUL, e, ir8)
let mir_add_offset e1 e2 = mir_add [e1; mir_offset e2]

(* MIR to retrieve length of an array *)
let mir_length e = `IRMem (`IRBinary (`IR_SUB, e, ir8))

(* MIR to implement a basic if statement *)
let mir_if e t_body f_body =
  let t = make_new_label () in
  let f = make_new_label () in
  `IRSeq [
    `IRCJump (e, t, f);
    `IRLabel t; t_body;
    `IRLabel f; f_body;
  ]

(* Equivalent to:
   for (int i = 0; i < n; ++i) {
     body_func i
   }
*)
let mir_for n body_func =
  let i = make_new_tmp () in
  let h = make_new_label () in
  `IRSeq [
    `IRMove (i, ir0);
    `IRLabel h;
    mir_if
      (`IRBinary (`IR_LT, i, n))
      (`IRSeq [
          body_func i;
          `IRMove (i, mir_add [i; ir1]);
          `IRJump (`IRName h);
        ])
      (`IRSeq []);
  ]

(* Helper to create memory with xi_alloc using MIR *)
let mir_alloc n =
  let arr = make_new_tmp () in
  `IRESeq (
    `IRSeq [
      `IRMove (
        arr,
        `IRCall (`IRName "_xi_alloc", [mir_offset (mir_add [n; ir1])], 1, 1)
      );
      `IRMove (`IRMem arr, n);
    ],
    mir_add [arr; ir8]
  )

(* Transforms AST expr node [e] to MIR expr node *)
let rec mir_of_expr ctx e : ir_expr =
  let l = location_of_node locations e in
  let mire = match e with
    | `Length e -> mir_length (mir_of_expr ctx e)
    | `Array es ->
      let n = `IRConst (List.length es |> Int64.of_int) in
      let arr = make_new_tmp () in
      `IRESeq (
        `IRSeq (
          `IRMove (arr, mir_alloc n)
          :: List.mapi es ~f:(fun i e ->
              `IRMove (
                `IRMem (mir_add_offset arr (`IRConst (Int64.of_int i))),
                mir_of_expr ctx e
              )
          )
        ),
        arr
      )
    | `Int i -> `IRConst (int64_of_bigint i)
    | `Bool b -> if b then ir1 else ir0
    | `String us ->
      let cs = List.map us ~f:(fun u -> `Char u) in
      mir_of_expr ctx (`Array cs)
    | `Char u -> `IRConst (Int64.of_int (Base.Uchar.to_scalar u))
    | `Var id ->
      begin
        match List.Assoc.find ctx id ~equal:(=) with
        | Some (`VarType _) -> `IRMem (`IRName id)
        | Some _ -> failwith "impossible"
        | None -> make_var id ctx
      end

    (* calling a method (i.e. dot expression *)
    | `ECall (`Binary (`Dot, e, `Var id), es) ->
      let (c_id, m_id) = String.lsplit2_exn id ~on:'.' in
      let `ClassType (_, _, _, meths) = Hash_table.find_exn class_map c_id in
      let (n_args, n_rets) = count_params_rets meths m_id in
      let vt = Hash_table.find_exn vt_map c_id in
      let index = Hash_table.find_exn vt m_id in
      let t = make_new_tmp () in
      `IRESeq (
        `IRMove (t, mir_of_expr ctx e),
        `IRCall (
          `IRMem (mir_add_offset (`IRMem t) (`IRConst (Int64.of_int index))),
          t :: List.map es ~f:(mir_of_expr ctx),
          (* keep arg/ret count here for ease of future stack setup *)
          (n_args + 1), n_rets
        )
      )

    (* calling a regular function *)
    | `ECall (`Var id, es) ->
      let (n_args, n_rets) = count_params_rets ctx id in
      `IRCall (`IRName id, List.map es ~f:(mir_of_expr ctx), n_args, n_rets)
        
    | `Unary (`Negate, `Int i) -> `IRConst (int64_of_bigint Bigint.(-i))
    | `Unary (`Negate, e) -> `IRBinary (`IR_SUB, ir0, mir_of_expr ctx e)
    | `Unary (`Not, e) -> `IRBinary (`IR_XOR, mir_of_expr ctx e, ir1)
    | `Unary (`New, `Var class_id) ->
      let obj = make_new_tmp () in
      `IRESeq (
        `IRSeq [
          `IRMove (
            obj,
            `IRCall (`IRName "_xi_alloc", [`IRMem (`IRName (size_of_class class_id))], 1, 1)
          );
          `IRMove (`IRMem obj, `IRName (vt_of_class class_id));
        ],
        obj
      )
        
    | `Binary (`Index, _, _) as binary -> mir_of_arr_index ctx binary
    (* accessing fields -- note that methods will never reach here due to the greedy nature
     * of pattern matching *)
    | `Binary (`Dot, e, `Var id) ->
      let (c_id, f_id) = String.lsplit2_exn id ~on:'.' in
      let layout = Hash_table.find_exn layout_map c_id in
      let index = Hash_table.find_exn layout f_id in
      `IRMem (
        mir_add_offset
          (`IRBinary (`IR_ADD, mir_of_expr ctx e, `IRMem (`IRName (size_of_class c_id))))
          (`IRConst (Int64.of_int index))
      )
    | `Binary _ as binary -> mir_of_binary ctx binary
    | `Null _ -> ir0
    | `This _ -> this_tmp
    | _ -> failwith "impossible expr irgen"
  in
  ir_add_location ir_locations (mire :> ir_node) l;
  mire

and mir_of_arr_index ctx (`Binary (`Index, e1, e2)) =
  (* NOTE: temps are necessary because e1 and e2 may have side effects *)
  let t1 = make_new_tmp () in
  let t2 = make_new_tmp () in
  (* Out of bounds if [t2 < 0] or [t2 >= len]. *)
  let len = `IRMem (`IRBinary (`IR_SUB, t1, ir8)) in
  let side_effects = `IRSeq [
      `IRMove (t1, mir_of_expr ctx e1);
      `IRMove (t2, mir_of_expr ctx e2);
      mir_if
        (`IRBinary (`IR_LT, t2, ir0))
        (`IRExp (`IRCall (`IRName "_xi_out_of_bounds", [], 0, 0)))
        (mir_if
           (`IRBinary (`IR_GEQ, t2, len))
           (`IRExp (`IRCall (`IRName "_xi_out_of_bounds", [], 0, 0)))
           (`IRSeq []))
    ] in
  `IRMem (`IRESeq (side_effects, mir_add_offset t1 t2))

and mir_of_binary ctx binary =
  let (`Binary (bop, e1, e2)) = binary in
  begin
    match bop with
    | `Concat ->
      let arr = make_new_tmp () in
      let l1 = make_new_tmp () in
      let l2 = make_new_tmp () in
      let a1 = make_new_tmp () in
      let a2 = make_new_tmp () in
      let n = make_new_tmp () in
      `IRESeq (
        `IRSeq [
          `IRMove (a1, mir_of_expr ctx e1);
          `IRMove (a2, mir_of_expr ctx e2);
          `IRMove (l1, mir_length a1);
          `IRMove (l2, mir_length a2);
          `IRMove (n, mir_add [l1; l2]);
          `IRMove (arr, mir_alloc n);
          mir_for l1 (fun i -> `IRMove (
              `IRMem (mir_add_offset arr i),
              `IRMem (mir_add_offset a1 i)
            ));
          mir_for l2 (fun i -> `IRMove (
              `IRMem (mir_add_offset arr (mir_add [l1; i])),
              `IRMem (mir_add_offset a2 i)
            ));
        ],
        arr
      )
    | `And ->
      (* Short-circuit *)
      let tmp = make_new_tmp () in
      let l1 = make_new_label () in
      let l2 = make_new_label () in
      let lf = make_new_label () in
      `IRESeq (
        `IRSeq [`IRMove (tmp, ir0);
                (* Nothing*) `IRCJump (mir_of_expr ctx e1, l1, lf);
                `IRLabel l1; `IRCJump (mir_of_expr ctx e2, l2, lf);
                `IRLabel l2; `IRMove (tmp, ir1);
                `IRLabel lf;
               ],
        tmp
      )

    | `Or ->
      (* Short-circuit *)
      let tmp = make_new_tmp () in
      let l1 = make_new_label () in
      let l2 = make_new_label () in
      let lf = make_new_label () in
      `IRESeq (
        `IRSeq [`IRMove (tmp, ir1);
                (* Nothing*) `IRCJump (mir_of_expr ctx e1, lf, l1);
                `IRLabel l1; `IRCJump (mir_of_expr ctx e2, lf, l2);
                `IRLabel l2; `IRMove (tmp, ir0);
                `IRLabel lf;
               ],
        tmp
      )
    | _ -> `IRBinary ((mir_of_binop bop), (mir_of_expr ctx e1), (mir_of_expr ctx e2))
  end

(* Boolean control flow logic *)
and mir_of_control ctx e t f =
  match e with
  | `Bool b -> `IRJump (`IRName (if b then t else f))
  | `Binary (`And, e1, e2) ->
    let t' = make_new_label () in
    `IRSeq [
      mir_of_control ctx e1 t' f;
      `IRLabel t'; mir_of_control ctx e2 t f;
    ]
  | `Binary (`Or, e1, e2) ->
    let f' = make_new_label () in
    `IRSeq [
      mir_of_control ctx e1 t f';
      `IRLabel f'; mir_of_control ctx e2 t f;
    ]
  | `Unary (`Not, e) -> mir_of_control ctx e f t
  | `Var _
  | `ECall _
  | `Binary _ -> `IRCJump (mir_of_expr ctx e, t, f)
  | _ -> failwith "this should never happen"

(* Given a type annotation, (e.g. int[e1][e2][][] or int or bool),
 * return a list of [`IRTemp]s that hold the value of each dimension
 * and the IR statements that store the dimensions into the temps. *)
and make_dim_tmps ctx ta =
  match ta with
  | `IntAnnot _
  | `BoolAnnot _
  | `ClassAnnot _
  | `ArrayAnnot (_, None) -> ([], [])
  | `ArrayAnnot (array_ta, Some e_dim) ->
    let dim_tmp = make_new_tmp () in
    let dim_tmp_tail, dim_tmp_init_stmts = make_dim_tmps ctx array_ta in
    dim_tmp :: dim_tmp_tail,
    `IRMove (dim_tmp, mir_of_expr ctx e_dim) :: dim_tmp_init_stmts

(* Create an MIR representation of type annotation [ta], moving the result into [e_dst].
 * Assume [dim_tmps] is a list of temps that contain the values for each dimension *)
and mir_of_type_annot e_dst ta dim_tmps =
  match ta with
  | `IntAnnot _
  | `BoolAnnot _
  | `ClassAnnot _
  | `ArrayAnnot (_, None) -> `IRSeq []
  | `ArrayAnnot (array_ta, Some _) ->
    let arr = make_new_tmp () in
    let dim_tmp = List.hd_exn dim_tmps in
    `IRSeq [
      `IRMove (arr, mir_alloc dim_tmp);
      mir_for dim_tmp (fun i ->
          mir_of_type_annot (`IRMem (mir_add_offset arr i)) array_ta (List.tl_exn dim_tmps)
        );
      `IRMove (e_dst, arr);
    ]

(* Transforms AST stmt node [s] to MIR stmt node.
 * [loop_label] specifies the label of the lexically enclosing loop (or None)
 * [ctx] is the global context that we use for global variables. *)
and mir_of_stmt ctx loop_label s : ir_stmt =
  match s with
  | `Assign (e1, e2) -> `IRMove (mir_of_expr ctx e1, mir_of_expr ctx e2)
  | `VarDecl ds ->
    let (`Decl (_, ta)) = List.hd_exn ds in
    let dim_tmps, dim_tmp_init_stmts = make_dim_tmps ctx ta in
    `IRSeq (
      dim_tmp_init_stmts
      @ List.map ds ~f:(fun (`Decl (id, ta)) -> mir_of_type_annot (make_var id ctx) ta dim_tmps)
    )
  | `VarInit (ds, e)  ->
    begin
      match e with
      | `ECall _ ->
        `IRSeq (
          `IRExp (mir_of_expr ctx e) ::
          (List.mapi ds ~f:(fun i d ->
               match d with
               | `Discard _ -> `IRSeq []
               | `Decl (id, _) ->
                 `IRMove ((make_var id ctx), `IRTemp (nth_ret i))
             ))
        )
      | _ ->
        if List.length ds <> 1 then failwith "should only be a single initialization" else
          let dcl = List.hd_exn ds in
          match dcl with
          | `Discard _ -> `IRSeq []
          | `Decl (id, _) -> `IRMove (make_var id ctx, mir_of_expr ctx e)
    end
  | `If (e, s) ->
    let t = make_new_label () in
    let f = make_new_label () in
    `IRSeq [
      mir_of_control ctx e t f;
      `IRLabel t; mir_of_stmt ctx loop_label s;
      `IRLabel f;
    ]
  | `IfElse (e, s1, s2) ->
    let t = make_new_label () in
    let f = make_new_label () in
    let end_label = make_new_label () in
    `IRSeq [
      mir_of_control ctx e t f;
      `IRLabel t; mir_of_stmt ctx loop_label s1; `IRJump (`IRName end_label);
      `IRLabel f; mir_of_stmt ctx loop_label s2;
      `IRLabel end_label;
    ]
  | `While (e, s) ->
    let h = make_new_label () in
    let t = make_new_label () in
    let f = make_new_label () in
    `IRSeq [
      `IRLabel h; mir_of_control ctx e t f;
      `IRLabel t; mir_of_stmt ctx (Some (`IRLabel f)) s; `IRJump (`IRName h);
      `IRLabel f;
    ]
  | `SCall (`Binary (`Dot, e, `Var id), es) ->
    let (c_id, m_id) = String.lsplit2_exn id ~on:'.' in
    let `ClassType (_, _, _, meths) = Hash_table.find_exn class_map c_id in
    let (n_args, n_rets) = count_params_rets meths m_id in
    let vt = Hash_table.find_exn vt_map c_id in
    let index = Hash_table.find_exn vt m_id in
    let t = make_new_tmp () in
    `IRSeq [
      `IRMove (t, mir_of_expr ctx e);
      `IRExp (
        `IRCall (
          `IRMem (mir_add_offset (`IRMem t) (`IRConst (Int64.of_int index))),
          t :: List.map es ~f:(mir_of_expr ctx),
          (n_args + 1), n_rets
        )
      )
    ]
  | `SCall (`Var id, es) ->
    let (n_args, n_rets) = count_params_rets ctx id in
    `IRExp (`IRCall (`IRName id, List.map es ~f:(mir_of_expr ctx), n_args, n_rets))
  | `Block ((ss, None)) -> `IRSeq (List.map ss ~f:(mir_of_stmt ctx loop_label))
  | `Block ((ss, Some (`Return rs))) ->
    let stmts = List.map ss ~f:(mir_of_stmt ctx loop_label) in
    let returns = List.map rs ~f:(mir_of_expr ctx) in
    `IRSeq (
      List.concat [
        stmts;
        [`IRReturn returns];
      ]
    )
  | `Break _ ->
    let (`IRLabel label_name) = Option.value_exn loop_label in
    `IRJump (`IRName label_name)
  | _ -> failwith "impossible stmt irgen"

(* Translate methods to valid MIR, adding "this" as the first argument for instance methods *)
let mir_of_meth ctx ?this:(this = false) (`Meth (id, ps, rtns, `Block (ss, r_opt))) =
  let param_ir =
    `IRSeq (
      (if this then [`IRMove (this_tmp, `IRTemp "_ARG0")] else [])
      @ List.mapi ps ~f:(fun i (`Param (id, _)) ->
          `IRMove (
            (make_var id ctx),
            `IRTemp (nth_arg (if this then i + 1 else i))
          )
        )
    )
  in
  let n_args = List.length ps + if this then 1 else 0 in
  let n_rets = List.length rtns in
  (* append empty return if there isn't one *)
  let r = Option.value r_opt ~default:(`Return []) in
  let body_ir = mir_of_stmt ctx None (`Block (ss, Some r)) in
  `IRFunc (id, `IRSeq [param_ir; body_ir], n_args, n_rets)

(* Make object layout and dispatch vector maps *)
let init_class_maps global_ctx =
  (* initialize vt for given class, initializing superclasses as necessary *)
  let rec make_vt c_id =
    let open Hash_table in
    find_or_add vt_map c_id ~default:(fun () ->
        let `ClassType (_, super_opt, _, meths) = find_exn class_map c_id in
        let vt = create (module String) in
        let s_vt = Option.value_map super_opt ~default:(create (module String)) ~f:make_vt in
        let s_vt_offset = Option.value_map super_opt ~default:0 ~f:(find_exn vt_size_map) in
        (* calculate the size of this vt by starting from the super's vt,
         * adding a space meant for the compiler, then appending all methods
         * that were not overrides of the super *)
        let vt_offset =
          List.fold meths ~init:s_vt_offset ~f:(fun vt_offset (m_id, _) ->
              let index = Option.value (find s_vt m_id) ~default:(vt_offset + 1) in
              set vt ~key:m_id ~data:index;
              max vt_offset index
            )
        in
        set vt_size_map ~key:c_id ~data:(vt_offset + 1);
        vt
      )
  in
  (* make object layout and dispatch vector maps *)
  List.(
    global_ctx
    |> filter_map ~f:(fun (id, t) ->
        match t with
        | `ClassType (_, _, fields, _) as c_type ->
          let field_map = Hash_table.create (module String) in
          List.iteri fields ~f:(fun i (f_id, _) ->
              Hash_table.set field_map ~key:f_id ~data:(i - List.length fields)
            );
          Hash_table.set layout_map ~key:id ~data:field_map;
          Hash_table.set class_map ~key:id ~data:c_type;
          Some id
        | _ -> None
      )
    |> iter ~f:(compose ignore make_vt)
  )

let defs_of_global_var (glob : global_var) : id list =
  match glob with
  | `VarDecl ds -> List.map ds ~f:(fun (`Decl (id, _)) -> id)
  | `VarInit (ds, _) ->
    List.filter_map ds ~f:(fun d ->
        match d with
        | `Discard _ -> None
        | `Decl (id, _) -> Some id
      )

let func_type_of_instance_method c_id m_id =
  let (`ClassType (_, _, _, meth_types)) = Hash_table.find_exn class_map c_id in
  List.Assoc.find_exn meth_types m_id ~equal:(=)

(* Copy the vtable from superclass [s_id] to subclass [cid] *)
let copy_vt ~super:s_id ~sub:c_id vt_size_map =
  let open Hash_table in
  mir_for
    (`IRConst (Int64.of_int (find_exn vt_size_map s_id)))
    (fun i_tmp ->
       `IRMove (
         `IRMem (mir_add_offset (`IRName (vt_of_class c_id)) i_tmp),
         `IRMem (mir_add_offset (`IRName (vt_of_class s_id)) i_tmp)
       )
    )

(* Move the function pointers for class [c_id] into the vtable *)
let fill_vt c_id vt_map =
  let open Hash_table in
  `IRSeq (
    c_id
    |> find_exn vt_map
    |> to_alist
    |> List.map ~f:(fun (m_id, index) ->
        let (`FuncType ft) = func_type_of_instance_method c_id m_id in
        `IRMove (
          `IRMem (
            mir_add_offset
              (`IRName (vt_of_class c_id)) (`IRConst (Int64.of_int index))
          ),
          `IRName (mangle_instance_method c_id m_id ft)
        )
      )
  )

(* Initialize classes (size and vt globals) *)
let init_class ~xi_class ~vt_map ~vt_size_map ~size_tmp =
  let (`Class (c_id, sup_opt, fields, _)) = xi_class in
  `IRSeq [
    begin
      match sup_opt with
      | None -> `IRMove (size_tmp, ir8);
      | Some s_id ->
        `IRSeq [
          (* Init super class and copy the super's vt to the current
             class' vt *)
          `IRExp (`IRCall (`IRName (init_of_class s_id), [], 0, 0));
          `IRMove (size_tmp, `IRMem (`IRName (size_of_class s_id)));
          copy_vt ~super:s_id ~sub:c_id vt_size_map
        ]
    end;
    (* Set the size of this class to: [size_of_super + 8 * num_fields] *)
    `IRMove (
      `IRMem (`IRName (size_of_class c_id)),
      mir_add_offset size_tmp (`IRConst (Int64.of_int (List.length fields)))
    );
    (* Add function pointers to the vt *)
    fill_vt c_id vt_map;
  ]

(* Initialize globals and classes *)
let mir_of_globals_classes ctx globals classes =
  let open Hash_table in
  let (glob_init, glob_sizes) =
    List.fold globals ~init:([], []) ~f:(fun (g_init_body, g_sizes) glob ->
        (* Translate global decl/init to statement, add to body of fn *)
        (* TODO: Make translation use the MEMORY location of the global variable *)
        mir_of_stmt ctx None (glob :> stmt) :: g_init_body,
        (* Get the defs of this global stmt and assign them all to 8, and add
           them to the existing global sizes *)
        (defs_of_global_var glob |> List.map ~f:(fun id -> (id, 8))) @ g_sizes
      )
    |> (fun (init_body, size_arrs) ->
        (* global initializer function that calls all the class initializers --
         * need to guarantee all classes are initialized before initializing
         * any global vars in case of global object variables *)
        List.(
          `IRFunc (
            "_I_global_init",
            `IRSeq (
              concat [
                map classes ~f:(fun (`Class (c_id, _, _, _)) ->
                    `IRExp (`IRCall (`IRName (init_of_class c_id), [], 0, 0))
                  );
                (init_body |> cons (`IRReturn []) |> rev);
              ]
            ),
            0, 0
          ), size_arrs |> rev
        )
      )
  in
  let (funcs, ctors, glob_sizes) =
    List.fold classes
      ~init:([], [glob_init], glob_sizes)
      ~f:(fun (funcs, ctors, g_sizes) (`Class (c_id, _, _, meths) as xi_class) ->
          let vt_size = find_exn vt_size_map c_id in
          let new_g_sizes = [(size_of_class c_id, 8); (vt_of_class c_id, vt_size * 8)] in
          let new_funcs =
            List.map meths ~f:(fun (`Meth (m_id, ps, rtns, b)) ->
                let (`FuncType ft) = func_type_of_instance_method c_id m_id in
                let meth = `Meth (mangle_instance_method c_id m_id ft, ps, rtns, b) in
                mir_of_meth ctx meth ~this:true
              )
          in
          (* class initializer function *)
          let new_ctor =
            let size_tmp = make_new_tmp () in
            `IRFunc (
              init_of_class c_id,
              `IRSeq [
                `IRMove (size_tmp, `IRMem (`IRName (size_of_class c_id)));
                (* If the size is still 0, then the class hasn't been initialized.
                   Otherwise, it has, meaning we can do nothing *)
                mir_if
                  (`IRBinary (`IR_EQ, size_tmp, `IRConst 0L))
                  (init_class ~xi_class ~vt_map ~vt_size_map ~size_tmp)
                  (`IRSeq []);
                `IRReturn [];
              ],
              0, 0
            )
          in
          (new_funcs @ funcs, new_ctor :: ctors, new_g_sizes @ g_sizes)
        )
  in
  (funcs, ctors, glob_sizes)

(* Translate entire program to MIR *)
let mir_of_program id global_ctx (`Program (_, globals, meths, classes)) =
  init_class_maps global_ctx;
  let top_level_funcs = List.map meths ~f:(mir_of_meth global_ctx) in
  let (class_funcs, ctors, glob_sizes) = mir_of_globals_classes global_ctx globals classes in
  `IRCompUnit (id, top_level_funcs @ class_funcs, ctors, glob_sizes)

(* Transforms an MIR expression [e] into lowered form *)
let rec lir_of_mir_expr (e : ir_expr) =
  let l = location_of_ir_node ir_locations e in
  let lire = match e with
    | `IRConst _
    | `IRName _
    | `IRTemp _ -> ([], e)
    | `IRMem e -> let (s_seq, e') = lir_of_mir_expr e in (s_seq, `IRMem e')
    | `IRESeq (s, e) ->
      let s_seq = lir_of_mir_stmt s in
      let (s_seq', e') = lir_of_mir_expr e in
      (s_seq @ s_seq', e')
    | `IRCall (`IRName id, eargs, n_args, n_rets) ->
      let tmp_lst = List.map eargs ~f:(fun _ -> make_new_tmp ()) in
      let side_effects =
        List.concat [
          List.concat (
            List.map2_exn eargs tmp_lst ~f:(fun ex tmp ->
                let (s_seq, e') = lir_of_mir_expr ex in
                s_seq @ [`IRMove (tmp, e')]
              )
          );
          [`IRExp (`IRCall (`IRName id, tmp_lst, n_args, n_rets))];
        ]
      in
      (side_effects, `IRTemp "_RET0")
    | `IRCall (ef, eargs, n_args, n_rets) ->
      let (sf, ef) = lir_of_mir_expr ef in
      let f_tmp = make_new_tmp () in
      let tmp_lst = List.map eargs ~f:(fun _ -> make_new_tmp ()) in
      let side_effects =
        List.concat [
          sf @ [`IRMove (f_tmp, ef)];
          List.concat (
            List.map2_exn eargs tmp_lst ~f:(fun ex tmp ->
                let (s_seq, e') = lir_of_mir_expr ex in
                s_seq @ [`IRMove (tmp, e')]
              )
          );
          [`IRExp (`IRCall (f_tmp, tmp_lst, n_args, n_rets))];
        ]
      in
      (side_effects, `IRTemp "_RET0")
    | `IRBinary (op, e1, e2) ->
      (* TODO: Figure out if we have to know if e1 and e2 commute.
       * For now, assume they never commute. *)
      let (s_seq1, e1') = lir_of_mir_expr e1 in
      let (s_seq2, e2') = lir_of_mir_expr e2 in
      let tmp = make_new_tmp () in
      (s_seq1 @ (`IRMove (tmp, e1') :: s_seq2), `IRBinary (op, tmp, e2'))
  in
  ir_add_location ir_locations (snd lire :> ir_node) l;
  lire

(* Transforms an MIR statement [s] into lowered form *)
and lir_of_mir_stmt (s : ir_stmt) =
  match s with
  | `IRLabel _ -> [s]
  | `IRReturn rs ->
    let s_seq, e_seq =
      List.fold rs
        ~f:
          (fun (ss, es) r ->
             let (s, e) = lir_of_mir_expr r in
             (ss @ s, e :: es))
        ~init:([], []) in
    s_seq @ [`IRReturn (List.rev e_seq)]
  | `IRSeq s_seq -> List.map s_seq ~f:lir_of_mir_stmt |> List.concat
  | `IRExp e -> fst (lir_of_mir_expr e)
  | `IRJump e ->
    let (s_seq, e') = lir_of_mir_expr e in
    s_seq @ [`IRJump e']
  | `IRCJump (e, l1, l2) ->
    let (s_seq, e') = lir_of_mir_expr e in
    s_seq @ [`IRCJump (e', l1, l2)]
  | `IRMove (e_dest, e_src) ->
    let (s_seq_src, e_src') = lir_of_mir_expr e_src in
    (* TODO: Figure out if we have to know if e_src affects the location of
     * e_dest. For now, assume it always does, and use the safe/general,
     * but less efficient translation *)
    begin
      match e_dest with
      | `IRTemp _ -> s_seq_src @ [`IRMove (e_dest, e_src')]
      | `IRMem e_mem ->
        let (s_seq_mem, e_mem') = lir_of_mir_expr e_mem in
        let tmp = make_new_tmp () in
        List.concat [
          s_seq_mem;
          [`IRMove (tmp, e_mem')];
          s_seq_src;
          [`IRMove (`IRMem tmp, e_src')];
        ]
      | _ -> failwith "impossible ir move"
    end

let lir_of_mir_func_decl (`IRFunc (id, `IRSeq ss, n_args, n_rets)) =
  `IRFunc (id, `IRSeq (List.concat (List.map ss ~f:lir_of_mir_stmt)), n_args, n_rets)

let lir_of_mir_comp_unit (`IRCompUnit (id, fds, ctors, glob_sizes)) =
  `IRCompUnit (
    id, List.map fds ~f:lir_of_mir_func_decl, List.map ctors ~f:lir_of_mir_func_decl, glob_sizes
  )

let print_diagnostic no_reorder lir cout =
  let canonical_str = canonical_of_lir no_reorder lir in
  Printf.fprintf cout "%s\n" canonical_str

let print_mir_diagnostic mir cout =
  let mir_str = sexp_of_ir_comp_unit mir |> Sexp.to_string_hum in
  Printf.fprintf cout "%s\n" mir_str

let main id global_ctx ((prog, locs) : program * loc_table) =
  debug 2 "generating IR...";
  copy_loc_table ~t_from:locs ~t_to:locations;
  let mir = mir_of_program id global_ctx prog in
  let lir = lir_of_mir_comp_unit mir in
  (mir, lir, ir_locations)

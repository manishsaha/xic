open Ast
open Common
open Core
open Cfg

let locations = empty_loc_table ()

(* Convert an OCaml bool [b] to a Xi bool *)
let ir_bool_of_bool b = if b then 1L else 0L

let ( *>> ) v1 v2 =
  let a = Bigint.of_int64 v1 in
  let b = Bigint.of_int64 v2 in
  let c = Bigint.(a * b) in
  let d = Bigint.(c asr 64) in
  Bigint.to_int64_exn d

(* Given an AST binary operation [b] on ints, return the corresponding
 * OCaml implementation of the operation *)
let fn_of_ast_binop_int b bx by =
  let x = Bigint.to_int64_exn bx in
  let y = Bigint.to_int64_exn by in
  Int64.(
    match b with
    | `Plus -> `Int (Bigint.of_int64 (x + y))
    | `Minus -> `Int (Bigint.of_int64 (x - y))
    | `Mult -> `Int (Bigint.of_int64 (x * y))
    | `HighMult -> `Int (Bigint.of_int64 (x *>> y))
    | `Div -> `Int (Bigint.of_int64 (x / y))
    | `Mod -> `Int (Bigint.of_int64 (x % y))
    | `Eq  -> `Bool (x =  y)
    | `Neq -> `Bool (x <> y)
    | `Leq -> `Bool (x <= y)
    | `Lt  -> `Bool (x <  y)
    | `Geq -> `Bool (x >= y)
    | `Gt  -> `Bool (x >  y)
    | _ ->
      failwith (Printf.sprintf "Binary operation %s does not operate on ints" (string_of_binop b))
  )

(* Given an AST binary operation [b] on bools, return the corresponding
 * OCaml implementation of the operation *)
let fn_of_ast_binop_bool b =
  match b with
  | `Eq -> (=)
  | `Neq -> (<>)
  | `And -> (&&)
  | `Or -> (||)
  | _ -> failwith (Printf.sprintf "Binary operation %s does not return a bool" (string_of_binop b))

let rec const_fold_ast_binary (`Binary (b, e1, e2)) =
  let e1' = const_fold_ast_expr e1 in
  let e2' = const_fold_ast_expr e2 in
  match (e1', e2') with
  | `Int _, `Int y when b = `Div && Bigint.(y = of_int 0) -> `Binary (b, e1', e2')
  | `Int x, `Int y -> fn_of_ast_binop_int b x y
  | `Bool b1, `Bool b2 -> `Bool ((fn_of_ast_binop_bool b) b1 b2)
  | _ -> `Binary (b, e1', e2')

(* Takes a typed AST unary expresion and does constant folding on it *)
and const_fold_ast_unary (`Unary (u, e1)) =
  let e1' = const_fold_ast_expr e1 in
  match (u, e1') with
  | `Negate, `Int i -> `Int (Bigint.(-i))
  | `Not, `Bool b -> `Bool (not b)
  | _ -> `Unary (u, e1')

(* Takes a typed AST expression and does constant folding on it *)
and const_fold_ast_expr e =
  let l = location_of_node locations e in
  let e' = match e with
  | `Int _
  | `Bool _
  | `String _ 
  | `Char _
  | `Var _
  | `Null _
  | `This _ -> e
  | `Length e -> `Length (const_fold_ast_expr e)
  | `Array es -> `Array (List.map es ~f:const_fold_ast_expr)
  | `ECall (ef, es) -> `ECall (const_fold_ast_expr ef, List.map es ~f:const_fold_ast_expr)
  | `Unary _ as u_exp -> const_fold_ast_unary u_exp
  | `Binary _ as b_exp -> const_fold_ast_binary b_exp
  in
  add_location locations (e' :> node) l;
  e'

(* Takes a type annotation [ta] and does constant folding, particularly when
 * the type is an `ArrayAnnot with some expression, e.g. x:int[5+2] *)
let rec const_fold_ast_type_annot d =
  match d with
  | `BoolAnnot _
  | `IntAnnot _
  | `ClassAnnot _
  | `ArrayAnnot (_, None) -> d
  | `ArrayAnnot (array_ta, Some e) ->
    let array_ta' = const_fold_ast_type_annot array_ta in
    let e' = const_fold_ast_expr e in
    `ArrayAnnot (array_ta', Some e')

(* Takes a typed AST statement [s] and does constant folding on it.
 * Note that NOPs are represented as an empty block. *)
let rec const_fold_ast_stmt s =
  let nop = `Block ([], None) in
  let l = location_of_node locations s in
  match s with
  | `Assign (e1, e2) ->
    let s' = `Assign (const_fold_ast_expr e1, const_fold_ast_expr e2) in
    add_location locations s' l;
    s'
  | `VarDecl ds ->
    let s' = `VarDecl (List.map ds ~f:(fun (`Decl (id, ta)) -> `Decl (id, const_fold_ast_type_annot ta))) in
    add_location locations s' l;
    s'
    | `VarInit (ds, e) ->
    let ds' =
      List.map ds ~f:(fun d ->
          match d with
          | `Discard _ -> d
          | `Decl (id, ta) ->
            let s' = `Decl (id, const_fold_ast_type_annot ta) in
            add_location locations s' l;
            s'
        )
    in
    let e' = const_fold_ast_expr e in
    let s' = `VarInit (ds', e') in
    add_location locations s' l;
    s'
  | `If (e, s1) ->
    let e' = const_fold_ast_expr e in
    let s1' = const_fold_ast_stmt s1 in
    begin
      match e' with
      | `Bool b -> if b then s1' else nop
      | _ ->
        let s' = `If (e', s1') in
        add_location locations s' l;
        s'
    end
  | `IfElse (e, s1, s2) ->
    let e' = const_fold_ast_expr e in
    let s1' = const_fold_ast_stmt s1 in
    let s2' = const_fold_ast_stmt s2 in
    begin match e' with
      | `Bool b -> if b then s1' else s2'
      | _ ->
        let s' = `IfElse (e', s1', s2') in
        add_location locations s' l;
        s'
    end
  | `While (e, s) ->
    let e' = const_fold_ast_expr e in
    let s' = const_fold_ast_stmt s in
    begin match e' with
      | `Bool false -> nop
      | _ ->
        let s'' = `While (e', s') in
        add_location locations s'' l;
        s''
    end
  | `SCall (ef, es) ->
    let s' = `SCall (const_fold_ast_expr ef, List.map es ~f:const_fold_ast_expr) in
    add_location locations s' l;
    s'
  | `Block (ss, r_opt) ->
    let ss' = List.map ss ~f:const_fold_ast_stmt in
    let r_opt' = Option.map r_opt ~f:(fun (`Return es) ->
        let es' = List.map es ~f:const_fold_ast_expr in
        `Return es'
      )
    in
    let b = `Block (ss', r_opt') in
    add_location locations b l;
    b
  | `Break _ -> s

(* Given an IR binary operation [b], return the corresponding
 * OCaml implementation of the operation *)
let fn_of_ir_binop b =
  let ir_bool_fn_of_bool_fn f x y = ir_bool_of_bool (f x y) in
  let open Int64 in
  match b with
  | `IR_ADD -> ( + )
  | `IR_SUB -> ( - )
  | `IR_MUL -> ( * )
  | `IR_HMUL -> ( *>> )
  | `IR_DIV -> ( / )
  | `IR_MOD -> ( % )
  | `IR_LSHIFT -> (fun x y -> shift_left x (to_int_trunc y))
  | `IR_RSHIFT -> (fun x y -> shift_right_logical x (to_int_trunc y))
  | `IR_ARSHIFT -> (fun x y -> shift_right x (to_int_trunc y))
  | `IR_AND -> bit_and
  | `IR_OR  -> bit_or
  | `IR_XOR -> bit_xor
  | `IR_EQ  -> ir_bool_fn_of_bool_fn ( = )
  | `IR_NEQ -> ir_bool_fn_of_bool_fn ( <> )
  | `IR_LT  -> ir_bool_fn_of_bool_fn ( < )
  | `IR_GT  -> ir_bool_fn_of_bool_fn ( > )
  | `IR_LEQ -> ir_bool_fn_of_bool_fn ( <= )
  | `IR_GEQ -> ir_bool_fn_of_bool_fn ( >= )

(* Takes an IR expression [e] and does constant folding on it. *)
let rec const_fold_ir_expr e =
  match e with
  | `IRConst _
  | `IRTemp _
  | `IRName _ -> e
  | `IRMem e_mem -> `IRMem (const_fold_ir_expr e_mem)
  | `IRESeq (s, e') ->
    `IRESeq (const_fold_ir_stmt s, const_fold_ir_expr e')
  | `IRCall (ef, es, n_args, n_rets) ->
    let ef = const_fold_ir_expr ef in
    let es = List.map es ~f:const_fold_ir_expr in
    `IRCall (ef, es, n_args, n_rets)
  | `IRBinary (op, e1, e2) ->
    let e1 = const_fold_ir_expr e1 in
    let e2 = const_fold_ir_expr e2 in
    begin
      match (e1, e2) with
      | (`IRConst n1, `IRConst n2) -> `IRConst (fn_of_ir_binop op n1 n2)
      | _ -> `IRBinary (op, e1, e2)
    end

(* Takes an IR statement [s] and does constant folding on it. *)
and const_fold_ir_stmt s =
  match s with
  | `IRLabel _ -> s
  | `IRSeq ss -> `IRSeq (List.map ss ~f:const_fold_ir_stmt)
  | `IRExp e -> `IRExp (const_fold_ir_expr e)
  | `IRJump e -> `IRJump (const_fold_ir_expr e)
  | `IRCJump (e, l1, l2) ->
    let e' = const_fold_ir_expr e in
    begin
      match e with
      | `IRConst 0L -> `IRJump (`IRName l2)
      | `IRConst _  -> `IRJump (`IRName l1)
      | _ -> `IRCJump (e', l1, l2)
    end
  | `IRMove (e_dest, e_src) ->
    `IRMove (const_fold_ir_expr e_dest, const_fold_ir_expr e_src)
  | `IRReturn es -> `IRReturn (List.map es ~f:const_fold_ir_expr)

let main_ast (module Config : ConfigSpec) ((`Program (uses, globals, meths, classes) as program), locs) =
  copy_loc_table ~t_from:locs ~t_to:locations;
  (* If specified, don't perform the optimization *)
  if not (Hash_table.find_exn Config.enabled_opts "cf") then
    (program, locs)
  else (
    debug 2 "folding AST constants...";
    let meths =
      List.map meths ~f:(fun (`Meth (id, ps, rtns, (`Block _ as b))) ->
          match const_fold_ast_stmt b with
          | `Block _ as b -> `Meth (id, ps, rtns, b)
          | _ -> failwith "const_fold block should always return block"
        )
    in
    (`Program (uses, globals, meths, classes), locations)
  )

let const_fold_func (module Config : ConfigSpec) (`IRFunc (fid, `IRSeq stmts, n_args, n_rets)) =
  let new_stmts = List.map stmts ~f:const_fold_ir_stmt in

  (* If specified, print the CFG after optimization *)
  if Hash_table.find_exn Config.optcfg_flags "cf" then (
    let (cfg, _, _, _) =
      Dataflow.generate_cfg
        ~values:new_stmts
        ~add_succs_func:Dataflow.add_ir_succs
        ~top_func:s_top_func
    in
    display_ir_set_graph cfg (Config.diag_out_path ^ "_" ^ fid ^ "_cf.dot");
  );

  `IRFunc (fid, `IRSeq new_stmts, n_args, n_rets)

let main_ir (module Config : ConfigSpec) (`IRCompUnit (id, fds, ctors, glob_inits) as ir) =
  (* If specified, don't perform the optimization *)
  if not (Hash_table.find_exn Config.enabled_opts "cf") then
    ir
  else (
    debug 2 "folding IR constants...";
    `IRCompUnit (id, List.map fds ~f:(const_fold_func (module Config)), ctors, glob_inits)
  )

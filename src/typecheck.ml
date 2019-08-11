open Ast
open Common
open Core

let rho = "__RHO__"
let beta = "__BETA__"  (* inside while loop *)
let mu = "__MU__"      (* inside class definition *)

(* let locations : (node * location) list ref = ref [] *)
let used_interfaces = Poly_set.create (module String)

let locations = empty_loc_table ()

let raise_semantic_error ?print:(print=true) err_msg ((f, l, c) as err_loc) =
  if print then 
    Printf.printf "Semantic error beginning at %s:%d:%d: %s\n" f l c err_msg;
  raise (Xic_error ({
      err_type = "Semantic";
      err_msg = err_msg;
      err_loc = err_loc;
    }))

(* Iterate through the hierarchy, applying function f to each class along the way *)
let rec hierarchy_iter id (ctx : context) ~f =
  match List.find ctx ~f:(fun (id', _) -> id = id') with
  | None -> ()
  | Some (_, env) ->
    begin match env with
      | `ClassType (c_id, ex_opt, _, _) ->
        f c_id;
        begin match ex_opt with
          | None -> ()
          | Some s_id -> hierarchy_iter s_id ctx ~f
        end
      | _ -> failwith "Found a non-class-type while iterating through the hierarchy"
    end

(* Find the LCA of just two types. Assumes t1 and t2 are base types.
   Returns None if there is none *)
let rec find_lca_pair ctx t1 t2 =
  match (t1, t2) with
  | _ when t1 = t2 -> Some t1
  | `AnyType, (_ as t)
  | (_ as t), `AnyType -> Some t
  | `ArrayLiteralType t1, `ArrayLiteralType t2 ->
    Option.map (find_lca_pair ctx t1 t2) ~f:(fun t -> `ArrayLiteralType t)
  | `ArrayLiteralType t1, `ArrayType t2
  | `ArrayType t2, `ArrayLiteralType t1 ->
    Option.map (find_lca_pair ctx t1 t2) ~f:(fun t -> `ArrayType t)
  | `ObjType c1_id, `ObjType c2_id ->
    let visited = Poly_set.create (module String) in
    hierarchy_iter c1_id ctx ~f:(fun id -> Poly_set.add visited id);
    With_return.with_return (fun r ->
        hierarchy_iter c2_id ctx ~f:(fun id ->
            if Poly_set.exists visited ~f:(fun id' -> id = id') then
              r.return (Some (`ObjType id))
            else
              Poly_set.add visited id
          );
        None
      )
  | _ -> None

(* Returns whether t1 is a subtype of t2 in the context. *)
let rec is_subtype ctx (t1 : valid_type) (t2 : valid_type) =
  match (t1, t2) with
  | _ when t1 = t2 -> true
  | (`AnyType, _)
  | (_, `AnyType) -> true
  | (`ArrayLiteralType t1, `ArrayType t2) -> is_subtype ctx (t1 :> valid_type) (t2 :> valid_type)
  | ((#base_type as t1), (#base_type as t2)) ->
    Option.value_map (find_lca_pair ctx t1 t2) ~f:((=) t2) ~default:false
  | _ -> false

(* Find the lowest common ancestor type of typed expression list *)
let find_lca ctx typed_es l =
  let types = List.map typed_es ~f:(fun (_, t) ->
      match t with
      | #base_type as t -> t
      | _ -> raise_semantic_error "type in typed expression wasn't a base type" l
    )
  in
  match types with
  | [] -> Some `AnyType
  | t :: tl_types ->
    List.fold_left tl_types ~init:(Some t) ~f:(fun curr_lca curr_t ->
        match curr_lca with
        | None -> None
        | Some lca -> find_lca_pair ctx lca curr_t
      )

(* diagnostic output functions *)
let print_diagnostic cout = Printf.fprintf cout "Valid Xi Program\n"

let print_locations locs =
  List.iter locs ~f:(fun (n,(f,l2,l3)) ->
      Printf.printf "%s %i,%i\t%s\n"
        f
        l2
        l3
        (string_of_ast_node n)
    )

(* Check if [id] is in [ctx]. *)
let has_symbol ctx id = List.Assoc.mem ctx id ~equal:(=)

(* Get the optional type of the given [id] in [ctx]. *)
let get_symbol ctx id = List.Assoc.find ctx id ~equal:(=)

(* Add a symbol to an existing context, where its location is in [node]
 * (in case the symbol is already defined).
 * Throws if the symbol [sym] is already defined in [ctx] *)
let add_symbol_exn ctx ((id, _) as sym) l =
  if has_symbol ctx id
  then raise_semantic_error (id ^ " already exists in context") l
  else sym :: ctx

(* Add a symbol to an existing context. Return the new context if symbol
 * [sym] does not exist in the context, else return [ctx]. *)
let add_symbol ctx ((id, _) as sym) =
  if has_symbol ctx id
  then ctx
  else sym :: ctx

(* Returns whether the [t1] and [t2] object comparison is valid. *)
let comparable ctx t1 t2 = 
  match (t1, t2, get_symbol ctx mu) with
  | (`ObjType c_id1, `ObjType c_id2, Some (`VarType (`ObjType c_id)))
    when c_id = c_id1 || c_id = c_id2 -> true
  | (#base_type, #base_type, _) -> true
  | _ -> false

(* Returns Some [class, base_type] if field was found in [c_id] or one 
 * of its super types. None if field does not exist in [c_id]'s hierarchy. *)
let valid_field ctx c_id field : (id * base_type) option = 
  let rec check_field c_id = 
    let class_header = get_symbol ctx c_id in
    match class_header with
    | Some (`ClassType (_, ex, ds, _)) ->
      begin
        let field_type = List.Assoc.find ds ~equal:(=) field in
        match field_type with
        | Some t -> Some (c_id, t)
        | None ->
          match ex with
          | Some ex_id -> check_field ex_id
          | None -> None
      end
    | _ -> failwith "this should never happen"
  in
  check_field c_id

(* Returns Some [class, class_type] if method was found in [c_id] or one 
 * of its super types. None if method does not exist in [c_id]'s hierarchy. *)
let valid_inst_meth ctx c_id meth_id : (id * func_type) option = 
  let rec check_inst_meth c_id = 
    let class_header = get_symbol ctx c_id in
    match class_header with
    | Some (`ClassType (_, ex, _, ms)) ->
      begin
        let field_type = List.Assoc.find ms ~equal:(=) meth_id in
        match field_type with
        | Some t -> Some (c_id, t)
        | None ->
          match ex with
          | Some ex_id -> check_inst_meth ex_id
          | None -> None
      end
    | _ -> failwith "this should never happen"
  in
  check_inst_meth c_id

let typecheck_inst_meth e ctx c_id m_id l = 
  match valid_inst_meth ctx c_id m_id with
  | Some (c_name, mt) -> (`Binary (`Dot, e, `Var (c_name ^ "." ^ m_id)), mt)
  | _ -> raise_semantic_error ("instance method " ^ m_id ^ " does not exist in " ^ c_id) l

let is_array_type t =
  match t with
  | `ArrayType _
  | `ArrayLiteralType _ -> true
  | _ -> false

(* Returns a typed AST expr node given input [e] and context [ctx]. *)
let rec typecheck_expr (ctx : context) (e : expr) : expr * valid_type =
  let l = location_of_node locations e in
  let (e, t) = 
    match e with
    | `Int _ -> (e, `IntType)
    | `Bool _ -> (e, `BoolType)
    | `String _ -> (e, `ArrayType `IntType)
    | `Char _ -> (e, `IntType)
    | `Var id -> 
      begin
        match (get_symbol ctx id, get_symbol ctx mu) with
        | (Some (`VarType t), _) -> (e, (t :> valid_type))
        (* | (Some (`FuncType _ as ft), _) -> (e, ft) *)
        | (None, Some (`VarType (`ObjType c_id))) ->
          begin
            match valid_field ctx c_id id with
            | Some (c_id, t) ->
              (`Binary (`Dot, `This (), `Var (c_id ^ "." ^ id)), (t :> valid_type))
            | _ -> raise_semantic_error ("field " ^ id ^ " does not exist in " ^ c_id) l
          end
        | _ -> raise_semantic_error ("found non-variable symbol " ^ id) l
      end
    (* check dot expressions to access field names *)
    | `Binary (`Dot, e, `Var f_id) ->
      let (e, et) = typecheck_expr ctx e in
      begin 
        match et with
        | `ObjType c_id -> 
          begin
            match valid_field ctx c_id f_id with
            | Some (c_name, ft) -> 
              (`Binary (`Dot, e, `Var (c_name ^ "." ^ f_id)), (ft :> valid_type))
            | _ -> raise_semantic_error ("field " ^ f_id ^ " does not exist in " ^ c_id) l
          end
        | _ -> raise_semantic_error ("found illegal dot expression") l
      end
    | `Binary (bop, e1, e2) -> 
      let (e1, et1) = typecheck_expr ctx e1 in
      let (e2, et2) = typecheck_expr ctx e2 in
      begin
        let e = `Binary (bop, e1, e2) in
        match (bop, et1, et2) with
        | (`Plus, `IntType, `IntType)
        | (`Minus, `IntType, `IntType)
        | (`Mult, `IntType, `IntType)
        | (`HighMult, `IntType, `IntType)
        | (`Div, `IntType, `IntType)
        | (`Mod, `IntType, `IntType) -> (e, `IntType)
        | (`Lt, `IntType, `IntType)
        | (`Leq, `IntType, `IntType)
        | (`Gt, `IntType, `IntType)
        | (`Geq, `IntType, `IntType)
        | (`And, `BoolType, `BoolType)
        | (`Or, `BoolType, `BoolType) -> (e, `BoolType)
        | (`Eq, _, _)
        | (`Neq, _, _) when comparable ctx et1 et2 -> (e, `BoolType)
        | (`Index, `ArrayLiteralType at, `IntType)
        | (`Index, `ArrayType at, `IntType) -> (e, (at :> valid_type))
        | (`Plus, (#base_type as t1), (#base_type as t2))
          when is_array_type t1 && is_array_type t2 ->
          begin
            match find_lca_pair ctx t1 t2 with
            | None -> raise_semantic_error "illegal operation" l
            | Some t -> (`Binary (`Concat, e1, e2), (t :> valid_type))
          end
        | _ -> raise_semantic_error "illegal operation" l 
      end
    | `Unary (`New, `Var c_id) -> (e, `ObjType c_id)
    | `Unary (uop, e) -> 
      let (e, et) = typecheck_expr ctx e in
      begin
        match (uop, et) with
        | (`Negate, `IntType)
        | (`Not, `BoolType) -> (`Unary (uop, e), (et :> valid_type))
        | _ -> raise_semantic_error "illegal operation" l
      end
    | `Length e -> 
      let (e, et) = typecheck_expr ctx e in
      if is_array_type et then (`Length e, `IntType) else
        raise_semantic_error "illegal length expression" l
    | `Array es ->
      (* Find the lowest common ancestor type of everything in the array *)
      let typed_es = List.map es ~f:(typecheck_expr ctx) in
      let lca = find_lca ctx typed_es l in
      begin
        match lca with
        | None -> raise_semantic_error "types of expressions in array have no common supertype" l
        | Some t -> (`Array (List.map typed_es ~f:fst), `ArrayLiteralType t)
      end
    | `ECall (ef, es) ->
      begin
        let (ef, `FuncType (pt, rt)) = 
          match ef with
          | `Binary (`Dot, e, `Var m_id) ->
            let (e, et) = typecheck_expr ctx e in
            begin 
              match et with
              | `ObjType c_id -> typecheck_inst_meth e ctx c_id m_id l
              | _ -> raise_semantic_error "found illegal dot expression" l
            end
          | `Var m_id ->
            begin
              match (get_symbol ctx m_id, get_symbol ctx mu) with
              | (Some (`FuncType _ as ft), _) -> (ef, ft)
              | (None, Some (`VarType (`ObjType c_id))) -> typecheck_inst_meth (`This ()) ctx c_id m_id l
              | _ -> raise_semantic_error ("cannot call symbol " ^ m_id ^ " as function") l
            end
          | _ -> raise_semantic_error "invalid function call" l
        in
        if rt <> `UnitType
        then
          let vrt = (rt :> valid_type) in
          match pt with
          | `UnitType -> 
            if List.length es = 0
            then (`ECall (ef, es), vrt)
            else raise_semantic_error "argument count mismatch" l
          | #base_type as pt ->
            begin
              match es with
              | [e] -> 
                let (e, et) = typecheck_expr ctx e in
                let l' = location_of_node locations e in
                if is_subtype ctx et pt
                then (`ECall (ef, [e]), vrt)
                else raise_semantic_error "argument type mismatch" l'
              | _ -> raise_semantic_error "argument count mismatch" l
            end
          | `TupleType pts ->
            begin
              try 
                let es = 
                  List.fold2_exn es pts ~init:[] ~f:(fun es e pt ->
                      let (e, et) = typecheck_expr ctx e in   
                      let l' = location_of_node locations e in
                      if is_subtype ctx et (pt :> valid_type)
                      then e :: es
                      else raise_semantic_error "argument type mismatch" l'
                    )
                in
                (`ECall (ef, List.rev es), vrt)
              with _ -> raise_semantic_error "argument count mismatch" l
            end
        else raise_semantic_error "procedure cannot be called as function" l
      end
    | `This _
    | `Null _ ->
      begin
        match get_symbol ctx mu with
        | Some ((`VarType (`ObjType c_id))) -> (e, `ObjType c_id)
        | _ -> 
          let e = string_of_expr e in
          raise_semantic_error (e ^ " cannot be used outside of class definition") l
      end
  in
  add_location locations (e :> node) l;
  (e, t)

(* Typechecks a type annotation with respect to the context. *)
let rec typecheck_type_annot ctx (ta : type_annot) =
  match ta with
  | `BoolAnnot _
  | `IntAnnot _
  | `ClassAnnot _ -> ta
  | `ArrayAnnot (array_ta, None) -> `ArrayAnnot (typecheck_type_annot ctx array_ta, None)
  | `ArrayAnnot (array_ta, Some e) ->
    let l' = location_of_node locations e in
    let (e, et) = typecheck_expr ctx e in
    if et <> `IntType
    then raise_semantic_error "invalid dimension type" l';
    `ArrayAnnot (typecheck_type_annot ctx array_ta, Some e)

(* Translates decl/param/ret_type_name [t] (from the AST) into a base_type. *)
let rec type_of_type_annot ctx ta : base_type =
  match ta with
  | `BoolAnnot _ -> `BoolType
  | `IntAnnot _ -> `IntType
  | `ArrayAnnot (array_ta, _) -> `ArrayType (type_of_type_annot ctx array_ta)
  | `ClassAnnot id -> `ObjType id

(* Returns a typed return statement given input return [r] and context [ctx]. *)
let typecheck_ret ctx (`Return es as r) =
  let l = location_of_node locations r in
  let (es, ets) =
    es
    |> List.map ~f:(typecheck_expr ctx)
    |> List.unzip
  in
  let check_return_types rts ets =
    try
      List.iter2_exn rts ets ~f:(fun rt et ->
          if not (is_subtype ctx et (rt :> valid_type))
          then raise_semantic_error "return type mismatch" l
        )
    with Invalid_argument _ ->
      raise_semantic_error "return type mismatch" l in
  match get_symbol ctx rho with
  | None -> raise_semantic_error "return type not found in context" l
  | Some (`ReturnType rt) ->
    begin
      match rt with
      | `UnitType ->
        if ets <> [] then raise_semantic_error "return type mismatch" l
      | `TupleType rts ->
        check_return_types rts ets
      | #base_type as rt -> check_return_types [rt] ets
    end;
    (`Return es, `VoidType, ctx)
  | Some _ -> raise_semantic_error "invalid return type in context" l

(* Returns the least upper bound of types [t1] and [t2].
 * Raises [Error] for non-unit, non-void types. *)
let lub t1 t2 =
  match (t1, t2) with
  | (`UnitType, `UnitType)
  | (`UnitType, `VoidType)
  | (`VoidType, `UnitType) -> `UnitType
  | (`VoidType, `VoidType) -> `VoidType
  | _ -> raise (Invalid_argument "invalid lub type")

(* Typechecks statement [s] in the given context [ctx].
 * Returns the typed statement and the new context. *)
let rec typecheck_stmt ctx (s : stmt) =
  let l = location_of_node locations s in
  let (s, t, ctx) =
    match s with
    | `Assign (e1, e2) ->
      let (e1, et1) = typecheck_expr ctx e1 in
      let (e2, et2) = typecheck_expr ctx e2 in
      if is_subtype ctx et2 et1
      then (`Assign (e1, e2), `UnitType, ctx)
      else raise_semantic_error "assign type mismatch" l
    | `VarDecl ds ->
      let (ctx, ds) =
        List.fold ds ~init:(ctx, []) ~f:(fun (ctx, ds) (`Decl (id, ta)) ->
            if has_symbol ctx id then raise_semantic_error (id ^ " identifier in use") l;
            let ta = typecheck_type_annot ctx ta in
            let t = type_of_type_annot ctx ta in
            (add_symbol_exn ctx (id, `VarType t) l, `Decl (id, ta) :: ds)
          )
      in
      (`VarDecl ds, `UnitType, ctx)
    | `VarInit (ds, e) ->
      let type_of_decl d =
        match d with
        | `Discard _ -> `AnyType
        | `Decl (_, ta) -> type_of_type_annot ctx ta in
      let dts = List.map ds ~f:type_of_decl in
      let (e, et) = typecheck_expr ctx e in
      let l' = location_of_node locations e in
      begin
        if List.mem dts `AnyType ~equal:(=)
        then 
          match e with
          | `ECall _ -> ()
          | _ -> raise_semantic_error "expected function call" l'
      end;
      let (ds, ctx) =
        List.fold ds ~init:([], ctx) ~f:(fun (ds, ctx) d ->
            match d with
            | `Discard _ -> (d :: ds, ctx)
            | `Decl (id, ta) ->
              let ta = typecheck_type_annot ctx ta in
              let t = type_of_type_annot ctx ta in
              (`Decl (id, ta) :: ds, add_symbol_exn ctx (id, `VarType t) (location_of_node locations d));
          )
        |> (fun (ds, ctx) -> (List.rev ds, ctx))
      in
      let check_types_match bts =
        try 
          List.iter2_exn bts dts ~f:(fun bt dt ->
              if not (is_subtype ctx (bt :> valid_type) (dt :> valid_type))
              then raise_semantic_error "variable type mismatch" l
            );
          (`VarInit (ds, e), `UnitType, ctx)
        with Invalid_argument _ ->
          raise_semantic_error "variable count mismatch" l
      in
      begin
        match et with
        | `TupleType bts -> check_types_match bts
        | #base_type as bt -> check_types_match [bt]
        | _ -> raise_semantic_error "variable type mismatch" l
      end
    | `If (e, s) ->
      let (e, et) = typecheck_expr ctx e in
      let (s, _, _) = typecheck_stmt ctx s in
      if et = `BoolType
      then (`If (e, s), `UnitType, ctx)
      else raise_semantic_error "if guard not boolean" l
    | `IfElse (e, s1, s2) ->
      let (e, et) = typecheck_expr ctx e in
      let (s1, st1, _) = typecheck_stmt ctx s1 in
      let (s2, st2, _) = typecheck_stmt ctx s2 in
      if et = `BoolType
      then (`IfElse (e, s1, s2), lub st1 st2, ctx)
      else raise_semantic_error "if else guard not boolean" l
    | `While (e, s) ->
      let ctx' = add_symbol ctx (beta, `BreakType) in
      let (e, et) = typecheck_expr ctx' e in
      let (s, _, _) = typecheck_stmt ctx' s in
      if et = `BoolType
      then (`While (e, s), `UnitType, ctx)
      else raise_semantic_error "while guard not boolean" l
    | `SCall (ef, es) -> 
      begin
        let (ef, eft) = 
          match ef with
          | `Binary (`Dot, e, `Var m_id) ->
            let (e, et) = typecheck_expr ctx e in
            begin 
              match et with
              | `ObjType c_id -> typecheck_inst_meth e ctx c_id m_id l
              | _ -> raise_semantic_error "found illegal dot expression" l
            end
          | `Var m_id ->
            begin
              match (get_symbol ctx m_id, get_symbol ctx mu) with
              | (Some (`FuncType _ as ft), _) -> (ef, ft)
              | (None, Some (`VarType (`ObjType c_id))) -> typecheck_inst_meth (`This ()) ctx c_id m_id l
              | _ -> raise_semantic_error ("cannot call symbol " ^ m_id ^ " as function") l
            end
          | _ -> raise_semantic_error "invalid function call" l
        in
        match eft with
        | `FuncType (`UnitType, `UnitType) ->
          if List.length es <> 0
          then raise_semantic_error "argument count mismatch" l
          else (`SCall (ef, es), `UnitType, ctx)
        | `FuncType ((#base_type as pt), `UnitType) ->
          (match es with
           | [e] -> 
             let (e, t) = typecheck_expr ctx e in
             if is_subtype ctx t pt
             then (`SCall (ef, [e]), `UnitType, ctx)
             else raise_semantic_error "argument type mismatch" l
           | _ -> raise_semantic_error "argument count mismatch" l)
        | `FuncType (`TupleType pts, `UnitType) ->
          begin
            try 
              let es = 
                List.fold2_exn es pts ~init:[] ~f:(fun es e pt ->
                    let l' = location_of_node locations e in
                    let (e, t) = typecheck_expr ctx e in
                    if is_subtype ctx t (pt :> valid_type)
                    then e :: es
                    else raise_semantic_error "argument type mismatch" l'
                  )
              in
              (`SCall (ef, List.rev es), `UnitType, ctx)
            with _ -> raise_semantic_error "argument count mismatch" l
          end
        | _ -> raise_semantic_error "function is not a procedure" l
      end
    | `Block (ss, None) ->
      let (ss, t, _) = typecheck_stmts ctx ss in
      (`Block (ss, None), t, ctx)
    | `Block (ss, Some r) ->
      let (ss, t, ctx') = typecheck_stmts ctx ss in
      if t = `VoidType
      then raise_semantic_error "unreachable return" (location_of_node locations r);
      let (r, t, _) = typecheck_ret ctx' r in
      (`Block (ss, Some r), t, ctx)
    | `Break () -> 
      if has_symbol ctx beta
      then (s, `VoidType, ctx)
      else raise_semantic_error "illegal break" l;
  in
  add_location locations (s :> node) l;
  (s, t, ctx)

(* Typechecks a list of statements [stmts] in [ctx].
 * Returns list of typed statements and a new context. *)
and typecheck_stmts ctx stmts =
  let (stmts, t, ctx) =
    List.fold stmts ~init:([], `UnitType, ctx) ~f:(fun (stmts, t, ctx) s ->
        if t = `VoidType
        then raise_semantic_error "unreachable statement" (location_of_node locations s);
        let (s, t, ctx) = typecheck_stmt ctx s in
        (s :: stmts, t, ctx)
      )
  in
  (List.rev stmts, t, ctx)

(* Typecheck AST nodes [nodes] using [typecheck_fn], passing contexts through.
 * [typecheck_fn] must return a typed AST node.
 * Returns a list of typed AST nodes in the same order as the input. *)
let typecheck_nodes ctx typecheck_fn nodes =
  let (nodes, ctx) =
    List.fold_left nodes ~init:([], ctx) ~f:(fun (nodes, ctx) node ->
        let (_, ctx) = typecheck_fn ctx node in
        (node :: nodes, ctx)
      )
  in
  (List.rev nodes, ctx)

(* Typechecks parameter [p] in context [ctx].
 * Returns a new typed parameter and new context. *)
let typecheck_param ctx (`Param (id, ta) as p) =
  let ta = typecheck_type_annot ctx ta in
  let t = type_of_type_annot ctx ta in
  (`Param (id, ta), add_symbol_exn ctx (id, `VarType t) (location_of_node locations p))

(* Returns a list of typed parameters given input [params] and context [ctx]. *)
let typecheck_params ctx params = typecheck_nodes ctx typecheck_param params

(* Typechecks declaration [d] in context [ctx].
 * Returns a new typed declaration and new context. *)
let typecheck_decl ctx (`Decl (id, ta) as d) =
  let ta = typecheck_type_annot ctx ta in
  let t = type_of_type_annot ctx ta in
  (`Decl (id, ta), add_symbol_exn ctx (id, `VarType t) (location_of_node locations d))

(* Returns a list of typed declarations given input [decls] and context [ctx]. *)
let typecheck_decls ctx decls = typecheck_nodes ctx typecheck_decl decls

(* Typechecks global [g] in context [ctx].
 * Returns a new typed global. *)
let typecheck_global ctx (g : global_var) = 
  let (global, _, ctx) = typecheck_stmt ctx (g :> stmt) in
  match g with
  | `VarDecl _
  | `VarInit _ -> (global, ctx)

(* Returns a list of typed globals given input [globals] and context [ctx]. *)
let typecheck_globals ctx globals = typecheck_nodes ctx typecheck_global globals

(* Transform a method signature into a function type (fn T -> T'). *)
let type_of_meth_sig ctx (ps : param list) (rtas : type_annot list) =
  let type_of_type_annots tas =
    match tas with
    | [] -> `UnitType
    | [ta] -> (type_of_type_annot ctx ta :> param_type)
    | _ -> `TupleType (List.map tas ~f:(type_of_type_annot ctx)) in
  let pt = type_of_type_annots (List.map ~f:(fun (`Param (_, ta)) -> ta) ps) in
  let rt = type_of_type_annots rtas in
  `FuncType (pt, rt)

(* Transform a class signature into a class type. *)
let type_of_class_sig ctx id ex ds ms = 
  let ds = List.map ds ~f:(fun (`Decl (id, ta)) -> 
      let t = type_of_type_annot ctx ta in
      (id, t))
  in
  let ms = List.map ms 
      ~f:(fun (`Meth (id, ps, rtas, _) | `MethSig (id, ps, rtas)) -> 
          (id, type_of_meth_sig ctx ps rtas)) in
  `ClassType (id, ex, ds, ms)

(* Adds a global's name and type to the context. *)
let add_global ctx (g : global_var) =
  let l = location_of_node locations g in
  match g with 
  | `VarInit (ds, _) -> 
    List.fold ds 
      ~init:ctx 
      ~f:(fun ctx d -> 
          match d with
          | `Discard () -> ctx
          | `Decl (id, ta) ->
            let t = type_of_type_annot ctx ta in
            add_symbol_exn ctx (id, `VarType t) (location_of_node locations d))
  | `VarDecl ds -> 
    List.fold ds
      ~init:ctx
      ~f:(fun ctx (`Decl (id, ta)) ->
          if has_symbol ctx id then raise_semantic_error (id ^ " identifier in use") l;
          let t = type_of_type_annot ctx ta in
          (add_symbol_exn ctx (id, `VarType t) l)
        )

(* Adds a function's name and type to the context. *)
let add_method lib_ctx ctx m =
  match m with
  | `Meth (id, ps, rtas, _)
  | `MethSig (id, ps, rtas) ->
    let msig_t = type_of_meth_sig ctx ps rtas in
    let l = location_of_node locations m in
    match List.Assoc.find lib_ctx id ~equal:(=) with
    | Some lib_t when lib_t <> msig_t -> raise_semantic_error "function implementation mismatch" l
    | _ -> add_symbol_exn ctx (id, type_of_meth_sig ctx ps rtas) l

(* Adds a class's name and type to the context. *)
let add_class lib_ctx ctx c =
  let add id s_opt ds ms =
    let csig_t = type_of_class_sig ctx id s_opt ds ms in
    let l = location_of_node locations c in
    begin
      match (List.Assoc.find lib_ctx id ~equal:(=) : env_type option) with
      | Some (`ClassType (_, s_opt', _, ms')) ->
        if (
          s_opt <> s_opt'
          || List.for_all ms' ~f:(fun (m_id, mt) ->
              List.mem ctx (m_id, (mt :> env_type)) ~equal:(=)
            )
        ) then raise_semantic_error "class implementation mismatch" l
      | Some _ -> raise_semantic_error ("symbol " ^ id ^ " already exists in context") l
      | None -> ()
    end;
    add_symbol_exn ctx (id, csig_t) l
  in
  match c with
  | `Class (id, s_opt, ds, ms) -> add id s_opt ds ms
  | `ClassSig (id, s_opt, ms) -> add id s_opt [] ms

(* Adds the program's globals, methods, and classes to the lib context, checking
 * shadowing first before checking implementation correctness. *)
let make_global_context lib_ctx ~meths ~classes = 
  let add_to_context ~lst ~f ctx = List.fold lst ~init:ctx ~f in
  [] 
  |> add_to_context ~lst:meths ~f:(add_method lib_ctx)
  |> add_to_context ~lst:classes ~f:(add_class lib_ctx)
  |> List.append lib_ctx

(* Returns a typed method signature given input [msig] and context [ctx].
 * Ensures shadowing of function and parameter names does not occur. *)
let typecheck_meth_sig ctx (`MethSig (id, ps, rtas) as msig : meth_sig) =
  let ctx = add_method [] ctx msig in
  let (ps, _) = typecheck_params ctx ps in
  (`MethSig (id, ps, rtas), ctx)

(* Typechecks a list of method signatures [msigs] in context [ctx].
 * Returns a list of typed method signatures and a new context.
 * Ensures that the names of the methods are unique. *)
let typecheck_meth_sigs ctx msigs = typecheck_nodes ctx typecheck_meth_sig msigs

(* Returns a typed class signature given input [csig] and context [ctx].
 * Ensures shadowing of class names does not occur. *)
let typecheck_class_sig ctx (`ClassSig (id, ex, msigs) as csig : xi_class_sig) =
  let ctx = add_class [] ctx csig in
  let (msigs, _) = typecheck_meth_sigs ctx msigs in
  (`ClassSig (id, ex, msigs), ctx)

(* Typechecks a list of class signatures [csigs] in context [ctx].
 * Returns a list of typed class signatures and a new context.
 * Ensures that the names of the classes are unique. *)
let typecheck_class_sigs ctx csigs = typecheck_nodes ctx typecheck_class_sig csigs

(* Typechecks a single "use" declaration by returning the new context after
 * including that interface. *)
let rec typecheck_use libs ctx (`Use id as use) =
  let loc = location_of_node locations use in
  Poly_set.add used_interfaces id;
  match List.Assoc.find libs id ~equal:(=) with
  | None -> raise_semantic_error ("library " ^ id ^ " not found") loc
  | Some path -> 
    try
      path
      |> Lex.main
      |> Parse.main_interface
      |> (fun (int, locs) -> add_locations locations locs; int)
      |> typecheck_interface libs ctx
    with Xic_error {err_type; err_msg; err_loc = (f, l, c)} ->
      raise_semantic_error
        (Printf.sprintf "invalid interface: %s error at %s:%d:%d: %s"
           (String.lowercase err_type) f l c err_msg) loc ~print:false

(* Typechecks a list of uses. *)
and typecheck_uses libs ctx uses = typecheck_nodes ctx (typecheck_use libs) uses

(* Typecheck an interface [int] in a given context [ctx].
 * Returns a typed interface and a new context. *)
and typecheck_interface libs ctx (`Interface (uses, msigs, csigs)) =
  let (_, ctx) = typecheck_uses libs ctx uses in
  let (msigs, ctx) = typecheck_meth_sigs ctx msigs in
  let (csigs, ctx) = typecheck_class_sigs ctx csigs in
  (`Interface (uses, msigs, csigs), ctx)

(* Typecheck a list of interfaces, making sure function names are disjoint.
 * Return a new list of typed interfaces.
 * NOTE: No need to pass a context because a list of interfaces always
 * comes at the beginning of files, where no context exists yet. *)
and typecheck_interfaces libs ints = typecheck_nodes [] (typecheck_interface libs) ints

(* Typecheck a method's body in the given context.
 * Return a new typed method. *)
let typecheck_meth ctx (`Meth (id, ps, rtas, (`Block _ as b)) as meth : meth) =
  let (`FuncType (_, rt)) = type_of_meth_sig ctx ps rtas in
  let l = location_of_node locations meth in
  let (ps, ctx) = typecheck_params ctx ps in
  let ctx = add_symbol_exn ctx (rho, `ReturnType rt) l in
  let (b, bt) =
    match typecheck_stmt ctx b with
    | (`Block _ as b, bt, _) -> (b, bt)
    | _ -> failwith "impossible meth block"
  in
  begin
    match (bt, rt) with
    | (_, `UnitType)
    | (`VoidType, _) -> ()
    | (`UnitType, _) -> raise_semantic_error "expected return" l
    | _ -> raise_semantic_error "impossible block type" l
  end;
  let m = `Meth (id, ps, rtas, b) in
  add_location locations m l;
  m

(* Typecheck a class' def'n in the given context. 
 * Return a new typed class and context with fields added. *)
let typecheck_class ctx (`Class (id, s_opt, fields, meths) as xi_class : xi_class) =
  let l = location_of_node locations xi_class in
  (* add special symbol mu in ctx to denote typechecker is in a class def'n *)
  let (fields, _) = typecheck_decls ctx fields in
  let ctx = add_symbol_exn ctx (mu, `VarType (`ObjType id)) l in
  let meths =
    List.map meths ~f:(fun (`Meth (id, _, _, _) as meth) -> 
        let l = location_of_node locations meth in
        (* prevent instance method from having the same name as top level methods *)
        if not (has_symbol ctx id)
        then typecheck_meth ctx meth
        else raise_semantic_error ("illegal method name " ^ id) l
      )
  in
  let c = `Class (id, s_opt, fields, meths) in
  add_location locations c l;
  c

let main_interface libs locs int =
  add_locations locations locs;
  typecheck_interface libs [] int

let main_program libs locs base_file (`Program (uses, globals, meths, classes) as p) =
  debug 2 "typechecking...";
  add_locations locations locs;
  let l = location_of_node locations p in
  let uses =
    match List.Assoc.find libs base_file ~equal:(=) with
    | None -> uses
    | Some _ ->
      let use = `Use base_file in
      add_location locations use (base_file, 1, 1);
      use :: uses
  in
  let (_, lib_ctx) = typecheck_uses libs [] uses in
  let global_ctx = make_global_context lib_ctx ~meths ~classes in
  let (globals, global_ctx) = typecheck_globals global_ctx globals in
  let classes = List.map classes ~f:(typecheck_class global_ctx) in
  let meths = List.map meths ~f:(typecheck_meth global_ctx) in
  let p = `Program (uses, globals, meths, classes) in
  add_location locations p l;
  (* print_locations !locations; *)
  ((p, global_ctx), locations)

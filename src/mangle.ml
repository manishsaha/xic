open Common
open Core
open Ast

let locations = empty_loc_table ()

let print_locations locs =
  List.iter locs ~f:(fun (n,(f,l2,l3)) ->
      Printf.printf "%s %i,%i\t%s\n"
        f
        l2
        l3
        (string_of_ast_node n)
    )

let rec encoding_of_base_type (bt : base_type) =
  match bt with
  | `AnyType -> failwith "`AnyType cannot be encoded"
  | `IntType -> "i"
  | `BoolType -> "b"
  | `ArrayType bt
  | `ArrayLiteralType bt -> "a" ^ encoding_of_base_type bt
  | `ObjType name -> "o" ^ Int.to_string (String.length name) ^ (encode_id name)

let encoding_of_ret_type rt =
  match rt with
  | #base_type as bt -> encoding_of_base_type bt
  | `UnitType -> "p"
  | `TupleType bts -> "t" ^ (Int.to_string (List.length bts)) ^ 
                      (List.map bts ~f:encoding_of_base_type |> String.concat)

let encoding_of_param_type rt =
  match rt with
  | #base_type as bt -> encoding_of_base_type bt
  | `UnitType -> ""
  | `TupleType bts -> List.map bts ~f:encoding_of_base_type |> String.concat

let mangle_instance_method c_id m_id (pt, rt) =
  let c_id_enc = encode_id c_id in
  let m_id_enc = encode_id m_id in
  let ret_enc = encoding_of_ret_type rt in
  let param_enc = encoding_of_param_type pt in
  "_I" ^ c_id_enc ^ "_" ^ m_id_enc ^ "_" ^ ret_enc ^ param_enc

let mangle_global var base_t =
  "_I_g_" ^ (encode_id var) ^ "_" ^ (encoding_of_base_type base_t)

(* Create a map from regular ids (of things in the context)*)
let make_mangle_map ctx =
  let map = Hash_table.create (module String) in
  List.iter ctx ~f:(fun (id, env) ->
      match env with
      | `ReturnType _ -> failwith "Tried to mangle something not supported by this function"
      | `VarType base_t -> Hash_table.set map ~key:id ~data:(mangle_global id base_t)
      | `FuncType (pt, rt) ->
        let name_enc = encode_id id in
        let ret_enc = encoding_of_ret_type rt in
        let param_enc = encoding_of_param_type pt in
        let fn_enc = "_I" ^ name_enc ^ "_" ^ ret_enc ^ param_enc in
        Hash_table.set map ~key:id ~data:fn_enc
      | _  -> ()
    );
  map

(* Mangle all function names in AST expr [e], using [map] as a mapping from
 * regular function names to mangled ones *)
let rec mangle_ast_expr map e =
  let l = location_of_node locations e in
  let e' = match e with
  | `Null _
  | `This _
  | `Int _
  | `Bool _
  | `String _
  | `Char _ -> e
  | `Var id ->
    let mangled_id = Hash_table.find map id |> Option.value ~default:id in
    `Var mangled_id
  | `Length e -> `Length (mangle_ast_expr map e)
  | `Array es -> `Array (List.map es ~f:(mangle_ast_expr map))
  | `Unary (uop, e) -> `Unary (uop, mangle_ast_expr map e)
  | `Binary (bop, e1, e2) ->
    let e1' = mangle_ast_expr map e1 in
    let e2' = mangle_ast_expr map e2 in
    `Binary (bop, e1', e2')
  | `ECall (ef, es) ->
    let mangled_ef = mangle_ast_expr map ef in
    let mangled_es = List.map es ~f:(mangle_ast_expr map) in
    `ECall (mangled_ef, mangled_es)
  in
  add_location locations (e' :> node) l;
  e'

let rec mangle_type_annot map ta =
  match ta with
  | `BoolAnnot _
  | `IntAnnot _
  | `ClassAnnot _
  | `ArrayAnnot (_, None) -> ta
  | `ArrayAnnot (array_ta, Some e) ->
    let array_ta = mangle_type_annot map array_ta in
    let e = mangle_ast_expr map e in
    `ArrayAnnot (array_ta, Some e)

let mangle_ast_ret map (`Return el) =
  `Return (List.map el ~f:(mangle_ast_expr map))

let rec mangle_ast_stmt map (s : stmt) : stmt =
  match s with
  | `Assign (e1, e2) ->
    let e1' = mangle_ast_expr map e1 in
    let e2' = mangle_ast_expr map e2 in
    `Assign (e1', e2')
  | `VarDecl ds ->
    `VarDecl (List.map ds ~f:(fun (`Decl (id, ta)) ->
        let mangled_id = Hash_table.find map id |> Option.value ~default:id in
        `Decl (mangled_id, mangle_type_annot map ta)))
  | `VarInit (dl, e) ->
    let e' = mangle_ast_expr map e in
    let dl' = List.map dl ~f:(fun d ->
        match d with
        | `Discard _ -> d
        | `Decl (id, ta) ->
          let mangled_id = Hash_table.find map id |> Option.value ~default:id in
          `Decl (mangled_id, mangle_type_annot map ta)
      )
    in
    `VarInit (dl', e')
  | `If (e, s) ->
    let e' = mangle_ast_expr map e in
    let s' = mangle_ast_stmt map s in
    `If (e', s')
  | `IfElse (e, s1, s2) ->
    let e' = mangle_ast_expr map e in
    let s1' = mangle_ast_stmt map s1 in
    let s2' = mangle_ast_stmt map s2 in
    `IfElse (e', s1', s2')
  | `While (e, s) ->
    let e' = mangle_ast_expr map e in
    let s' = mangle_ast_stmt map s in
    `While (e', s')
  | `SCall (ef, es) ->
    let mangled_ef = mangle_ast_expr map ef in
    let mangled_es = List.map es ~f:(mangle_ast_expr map) in
    `SCall (mangled_ef, mangled_es)
  | `Block ((s_body, ret_opt)) ->
    let s_body' = List.map s_body ~f:(mangle_ast_stmt map) in
    let ret_opt' = Option.map ret_opt ~f:(mangle_ast_ret map) in
    `Block ((s_body', ret_opt'))
  | `Break () -> s

let mangle_ast_meth map (`Meth (id, ps, rtas, (`Block _ as b))) =
  let mangled_id = Hash_table.find map id |> Option.value ~default:id in
  let b' =
    match mangle_ast_stmt map b with
    | `Block _ as b' -> b'
    | _ -> failwith "impossible mangle block"
  in
  `Meth (mangled_id, ps, rtas, b')

let mangle_ast_class map (`Class (c_id, s_opt, fields, meths)) =
  `Class (c_id, s_opt, fields, List.map meths ~f:(mangle_ast_meth map))

let mangle_global_vars map global : global_var =
  match mangle_ast_stmt map (global :> stmt) with
  | #global_var as mangled_global -> mangled_global
  | _ -> failwith "Global variable was mangled into a different type"
  (* match global with
   * | `VarDecl ds -> `VarDecl (
   *     List.map ds ~f:(fun (`Decl (id, ta)) ->
   *         let mangled_id = Hash_table.find map id |> Option.value ~default:id in
   *         `Decl (mangled_id, ta)
   *       )
   *   )
   * | `VarInit (ds, e) -> `VarInit (
   *     List.map ds ~f:(fun d ->
   *         match d with
   *         | `Decl (id, ta) ->
   *           let mangled_id = Hash_table.find map id |> Option.value ~default:id in
   *           `Decl(mangled_id, ta)
   *         | `Discard () -> d
   *       ),
   *     e
   *   ) *)


let mangle_ast_program (`Program (us, globals, meths, classes)) (global_ctx : context) =
  let map = make_mangle_map global_ctx in
  let meths = List.map meths ~f:(mangle_ast_meth map) in
  let classes = List.map classes ~f:(mangle_ast_class map) in
  let globals = List.map globals ~f:(mangle_global_vars map) in
  (
    `Program (us, globals, meths, classes),
    List.concat [
      [
        ("_xi_alloc", (`FuncType (`IntType, `IntType) :> env_type));
        ("_xi_out_of_bounds", (`FuncType (`UnitType, `UnitType) :> env_type));
      ];
      List.map global_ctx ~f:(fun (id, t) ->
          let mangled_id = Hash_table.find map id |> Option.value ~default:id in
          (mangled_id, (t :> env_type))
        );
    ]
  )

let main (((prog, global_ctx), locs) : (program * context) * loc_table)
  : (program * loc_table) * context =
  copy_loc_table ~t_from:locs ~t_to:locations;
  let (mangled, globaled) = mangle_ast_program prog global_ctx in
  ((mangled, locations), globaled)

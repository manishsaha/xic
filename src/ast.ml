open Common
open Core

type base_type = [
  | `AnyType
  | `ArrayLiteralType of base_type
  | `IntType
  | `BoolType
  | `ArrayType of base_type
  | `ObjType of id
]

(* T *)
type param_type = [
  | base_type
  | `UnitType
  | `TupleType of base_type list
]

(* T alias *)
type ret_type = param_type

(* R *)
type stmt_type = [
  | `UnitType
  | `VoidType
]

type func_type = [
  | `FuncType of param_type * ret_type
]

(* xi_class_type is like a xi_class_sig except
   - the fields are mappings from their name to a base type,
   - and method signatures are mappings from their name to a func type *)
type xi_class_type = [
  | `ClassType of id * id option * (id * base_type) list * (id * func_type) list
]

(* sigma *)
type env_type = [
  | `VarType of base_type
  | `ReturnType of ret_type
  | `BreakType
  | func_type
  | xi_class_type
]

type valid_type = [ param_type | ret_type | stmt_type | env_type ]

type symbol = (id * env_type)
type context = symbol list

type hierarchy = (id, id) Hash_table.t

(* AST operator types *)
type unop = [
  | `Negate
  | `Not
  | `New
]

type binop = [
  | `Plus
  | `Minus
  | `Mult
  | `HighMult
  | `Div
  | `Mod
  | `Eq
  | `Neq
  | `Leq
  | `Lt
  | `Geq
  | `Gt
  | `And
  | `Or
  | `Index
  | `Concat
  | `Dot
]

type expr = [
  | `Length of expr
  | `Array of expr list
  | `Int of Bigint.t
  | `Bool of bool
  | `String of escaped_char list
  | `Char of escaped_char
  | `Var of id
  | `ECall of expr * expr list
  | `Unary of unop * expr
  | `Binary of binop * expr * expr
  | `Null of unit
  | `This of unit
]

type type_annot = [
  | `BoolAnnot of unit
  | `IntAnnot of unit
  | `ArrayAnnot of type_annot * expr option
  | `ClassAnnot of id
]

type decl = [ `Decl of id * type_annot ]
type discard = [ `Discard of unit ]

type var_decl = [
  | `VarDecl of decl list
]

type var_init = [
  | `VarInit of [ decl | discard ] list * expr
]

(* Global variable can be initialized or uninitialized *)
type global_var = [
  | var_decl
  | var_init
]

type return = [ `Return of expr list ]

type stmt = [
  | `Assign of expr * expr
  | var_decl
  | var_init
  | `If of expr * stmt
  | `IfElse of expr * stmt * stmt
  | `While of expr * stmt
  | `SCall of expr * expr list
  | `Block of stmt list * return option
  | `Break of unit
]

type param = [
  | `Param of id * type_annot
]

type meth = [
  | `Meth of id * param list * type_annot list * [ `Block of stmt list * return option ]
]

type meth_sig = [
  | `MethSig of id * param list * type_annot list
]

type class_member = [
  | var_decl
  | meth
]

(* name of class, optional superclass, list of fields, list of methods *)
type xi_class = [
  | `Class of id * id option * decl list * meth list
]

(* name of class, optional superclass, list of fields, list of method sigs *)
type xi_class_sig = [
  | `ClassSig of id * id option * meth_sig list
] [@@deriving variants]

type use = [
  | `Use of id
]

(* uses, method sigs, class sigs *)
type interface = [
  | `Interface of use list * meth_sig list * xi_class_sig list
]

type program_member = [
  | global_var
  | meth
  | xi_class
]

(* uses, global variables, methods, classes *)
type program = [
  | `Program of use list * global_var list * meth list * xi_class list
]

type root = [ interface | program ]

type node = [
  | root
  | use
  | xi_class_sig
  | xi_class
  | meth
  | meth_sig
  | param
  | stmt
  | return
  | type_annot
  | discard
  | decl
  | expr
]

type locations = (node * location) list

let rec string_of_type t =
  match t with
  | `AnyType -> "any"
  | `IntType -> "int"
  | `BoolType -> "bool"
  | `ArrayLiteralType t -> ">=" ^ string_of_type (t :> valid_type) ^ "[]"
  | `ArrayType t -> string_of_type (t :> valid_type) ^ "[]"
  | `BreakType -> "break"
  | `UnitType -> "unit"
  | `VoidType -> "void"
  | `TupleType ts ->
    List.fold
      ~f:(fun s t -> s ^ ", " ^ string_of_type (t :> valid_type)) ~init:"(" ts
    ^ ")"
  | `VarType t -> "var " ^ string_of_type (t :> valid_type)
  | `ReturnType t -> "return " ^ string_of_type (t :> valid_type)
  | `FuncType (pt, rt) ->
    "fn " ^ string_of_type (pt :> valid_type) ^ " -> " ^ string_of_type (rt :> valid_type)
  | `ObjType id -> id
  | `ClassType (id, _, _, _) -> id

let string_of_unop uop =
  match uop with
  | `Negate -> "-"
  | `Not -> "!"
  | `New -> "new"

let string_of_binop bop =
  match bop with
  | `Plus -> "+"
  | `Minus -> "-"
  | `Mult -> "*"
  | `HighMult -> "*>>"
  | `Div -> "/"
  | `Mod -> "%"
  | `Eq -> "=="
  | `Neq -> "!="
  | `Leq -> "<="
  | `Lt -> "<"
  | `Geq -> ">="
  | `Gt -> ">"
  | `And -> "&"
  | `Or -> "|"
  | `Index -> "[]"
  | `Concat -> "^"
  | `Dot -> "."

let rec string_of_expr e =
  match e with
  | `Length e ->
    Printf.sprintf "(length %s)" (string_of_expr e)
  | `Array es ->
    Printf.sprintf "(%s)" (spacify_map ~f:string_of_expr es)
  | `Int i -> Bigint.to_string i
  | `Bool b -> Bool.to_string b
  | `String us -> "\"" ^ string_of_uchar_list us ^ "\""
  | `Char u -> "\'" ^ string_of_uchar u ^ "\'"
  | `Var id -> id
  | `ECall (e, es) ->
    Printf.sprintf "(%s)" (spacify_map (e :: es) ~f:string_of_expr)
  | `Unary (uop, e) ->
    Printf.sprintf "(%s %s)" (string_of_unop uop) (string_of_expr e)
  | `Binary (bop, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_binop bop) (string_of_expr e1) (string_of_expr e2)
  | `Null _ -> "null"
  | `This _ -> "this"

let rec string_of_type_annot ta =
  match ta with
  | `BoolAnnot _ -> "bool"
  | `IntAnnot _ -> "int"
  | `ArrayAnnot (t', None) -> Printf.sprintf "([] %s)" (string_of_type_annot t')
  | `ArrayAnnot (t', Some e) ->
    Printf.sprintf "([] %s %s)" (string_of_type_annot t') (string_of_expr e)
  | `ClassAnnot id -> id

let string_of_decl d =
  match d with
  | `Discard () -> "_"
  | `Decl (id, ta) -> Printf.sprintf "(%s %s)" id (string_of_type_annot ta)

let string_of_decls ds =
  match ds with
  | [] -> "()"
  | [d] -> string_of_decl d
  | _ -> Printf.sprintf "(%s)" (spacify_map ds ~f:string_of_decl)

let string_of_return (`Return es) =
  Printf.sprintf "(%s)" (spacify_list ("return"::List.map ~f:string_of_expr es))

let rec string_of_stmt stmt =
  match stmt with
  | `Assign (e1, e2) ->
    Printf.sprintf "(= %s %s)" (string_of_expr e1) (string_of_expr e2)
  | `VarDecl ds -> string_of_decls ds
  | `VarInit (ds, e) ->
    Printf.sprintf "(= %s %s)" (string_of_decls ds) (string_of_expr e)
  | `If (e, s) ->
    Printf.sprintf "(if %s %s)" (string_of_expr e) (string_of_stmt s)
  | `IfElse (e, st, sf) ->
    Printf.sprintf "(if %s %s %s)" (string_of_expr e) (string_of_stmt st) (string_of_stmt sf)
  | `While (e, s) ->
    Printf.sprintf "(while %s %s)" (string_of_expr e) (string_of_stmt s)
  | `SCall (ef, es) -> 
    Printf.sprintf "(%s)" (spacify_map (ef :: es) ~f:string_of_expr)
  | `Block b -> string_of_block b
  | `Break _ -> "break"

and string_of_block (stmts, ret_opt) =
  Printf.sprintf "(%s)"
    (spacify_list
       (List.map ~f:string_of_stmt stmts @
        match ret_opt with 
        | None -> []
        | Some ret -> [string_of_return ret]))

let string_of_param (`Param (id, ta)) = Printf.sprintf "(%s %s)" id (string_of_type_annot ta)

let string_of_meth (`Meth (id, ps, rtas, `Block b)) =
  Printf.sprintf
    "(%s (%s) (%s) %s)"
    id
    (spacify_map ~f:string_of_param ps)
    (spacify_map ~f:string_of_type_annot rtas)
    (string_of_block b)

let string_of_use (`Use id) = Printf.sprintf "(use %s)" id

let string_of_meth_sig (`MethSig (id, ps, rtas)) =
  Printf.sprintf
    "(%s (%s) (%s))"
    id
    (spacify_map ~f:string_of_param ps)
    (spacify_map ~f:string_of_type_annot rtas)

let string_of_class_sig (`ClassSig (id, sup_opt, msigs)) =
  match sup_opt with
  | None ->
    Printf.sprintf "(%s (%s))"
      id
      (spacify_list (List.map msigs ~f:string_of_meth_sig))
  | Some sup ->
    Printf.sprintf "(%s %s (%s))"
      id sup
      (spacify_list (List.map msigs ~f:string_of_meth_sig))

let string_of_class (`Class (id, sup_opt, decls, meths)) =
  match sup_opt with
  | None ->
    Printf.sprintf "(%s (%s) (%s))"
      id
      (spacify_list (List.map decls ~f:string_of_decl))
      (spacify_list (List.map meths ~f:string_of_meth))
  | Some sup ->
    Printf.sprintf "(%s %s (%s) (%s))"
      id sup
      (spacify_list (List.map decls ~f:string_of_decl))
      (spacify_list (List.map meths ~f:string_of_meth))

let string_of_root root =
  match root with
  | `Program (uses, [], meths, []) ->
    Printf.sprintf "((%s) (%s))"
      (spacify_map ~f:string_of_use uses)
      (spacify_map ~f:string_of_meth meths)
  | `Program (uses, globals, meths, classes) ->
    Printf.sprintf "((%s) (%s) (%s) (%s))"
      (spacify_map ~f:string_of_use uses)
      (spacify_map ~f:string_of_stmt globals)
      (spacify_map ~f:string_of_meth meths)
      (spacify_map ~f:string_of_class classes)
  | `Interface ([], msigs, []) ->
    Printf.sprintf "((%s))" (spacify_map ~f:string_of_meth_sig msigs)
  | `Interface (uses, msigs, csigs) ->
    Printf.sprintf "((%s) (%s) (%s))"
      (spacify_map ~f:string_of_use uses)
      (spacify_map ~f:string_of_meth_sig msigs)
      (spacify_map ~f:string_of_class_sig csigs)

let string_of_ast_node (node : node) =
  match node with
  | #root as root -> string_of_root root
  | #use as use -> string_of_use use
  | #meth_sig as msig -> string_of_meth_sig msig
  | #meth as meth -> string_of_meth meth
  | #param as param -> string_of_param param
  | #stmt as stmt -> string_of_stmt stmt
  | #return as ret -> string_of_return ret
  | #type_annot as ta -> string_of_type_annot ta
  | #decl 
  | #discard as decl -> string_of_decl decl
  | #expr as expr -> string_of_expr expr
  | #xi_class_sig as xcsig -> string_of_class_sig xcsig
  | #xi_class as xc -> string_of_class xc

let count_params t =
  match t with
  | #base_type -> 1
  | `UnitType -> 0
  | `TupleType ts -> List.length ts

let count_params_rets ctx f =
  match List.Assoc.find_exn ctx f ~equal:(=) with
  | `FuncType (pt, rt) -> (count_params pt, count_params rt)
  | _ -> failwith "impossible context type in reg alloc"

(* Translates decl/param/ret_type_name [t] (from the AST) into a base_type. *)
let rec type_of_type_annot ta : base_type =
  match ta with
  | `BoolAnnot () -> `BoolType
  | `IntAnnot () -> `IntType
  | `ArrayAnnot (array_ta, _) -> `ArrayType (type_of_type_annot array_ta)
  | `ClassAnnot id -> `ObjType id

(* Transform a method signature into a function type (fn T -> T'). *)
let type_of_meth_sig ptns rtns =
  let type_of_type_annots tas : param_type =
    match tas with
    | [] -> `UnitType
    | [ta] -> (type_of_type_annot ta :> param_type)
    | _ -> `TupleType (List.map ~f:type_of_type_annot tas) in
  let pt = type_of_type_annots (List.map ~f:(fun (`Param (_, tn, _)) -> tn) ptns) in
  let rt = type_of_type_annots rtns in
  `FuncType (pt, rt)

module AstLocTable = Hash_table.Make(struct
    type t = node
    let equal = phys_equal
    let compare n1 n2 = String.compare (string_of_ast_node n1) (string_of_ast_node n2)
    let hash n = Hash_table.hash (string_of_ast_node n)
    let sexp_of_t n = sexp_of_string (string_of_ast_node n)
    let t_of_sexp _ = (`Null ())
  end)

type loc_table = location AstLocTable.t

(* Returns the location (line, col) of a node in the AST *)
let location_of_node tbl (#node as node) =
  match AstLocTable.find tbl node with
  | Some l -> l
  | None -> ("fails: " ^ string_of_ast_node node, -1, -1)
(* print_locations !locations;
 * failwithf "cannot find location_of_node %s" (string_of_ast_node node) () *)

let add_location tbl n l =
  AstLocTable.set tbl ~key:n ~data:l

let add_locations tbl locs =
  List.iter locs ~f:(fun (n, l) -> add_location tbl n l)

let copy_loc_table ~t_from ~t_to =
  AstLocTable.iteri t_from ~f:(fun ~key ~data -> add_location t_to key data)

let empty_loc_table () = AstLocTable.create ()

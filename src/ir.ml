open Common
open Core

type ir_op =  [
  | `IR_ADD | `IR_SUB | `IR_MUL | `IR_HMUL | `IR_DIV | `IR_MOD
  | `IR_LSHIFT | `IR_RSHIFT | `IR_ARSHIFT
  | `IR_AND | `IR_OR | `IR_XOR
  | `IR_EQ | `IR_NEQ | `IR_LT | `IR_GT | `IR_LEQ | `IR_GEQ
] [@@deriving sexp]

(* `IRConst n - a 64-bit signed integer constant
 * `IRTemp t - a temporary register
 * `IRBinary (ir_op, e1, e2) - result of applying [ir_op] to [e1] and [e2]
 * `IRMem e - the contents of memory location with address [e]
 * `IRCall (e_f, e_lst) - result of executing a function, located at memory
 *                        address [e_f], with arguments [e_lst]
 * `IRName n - address of the memory location of [`IRLabel n]
 * `IRESeq (s, e) - result of [e] after statement [s] is executed
 **)
type ir_expr = [
  | `IRConst of int64
  | `IRTemp of id
  | `IRBinary of ir_op * ir_expr * ir_expr
  | `IRMem of ir_expr
  | `IRCall of ir_expr * ir_expr list * int * int
  | `IRName of id
  | `IRESeq of ir_stmt * ir_expr
] [@@deriving sexp]

(* `IRMove (dst, e) - moves the result of [e] into dst
 *     - [dst] should be either [`IRTemp t] or [`IRMem e']
 * `IRExp e - evaluate [e] for side effects, discard result
 * `IRSeq [s1;...;sn] - execute all statements in order
 * `IRJump e - jump to address [e]
 * `IRCJump e l1 l2 - jump to [l1] if [e] is true, else jump to [l2]
       - [l1] and [l2] should be the id of some `IRLabel
 * `IRLabel n - a label (for use in `IRName)
 * `IRReturn - returns from this function
 **)
and ir_stmt = [
  | `IRMove of ir_expr * ir_expr
  | `IRExp of ir_expr
  | `IRSeq of ir_stmt list
  | `IRJump of ir_expr
  | `IRCJump of ir_expr * id * id
  | `IRLabel of id
  | `IRReturn of ir_expr list
] [@@deriving sexp]

type ir_func_decl = [
  | `IRFunc of id * [ `IRSeq of ir_stmt list ] * int * int
] [@@deriving sexp]

(* name, methods, ctors, global sizes *)
type ir_comp_unit = [
  | `IRCompUnit of id * ir_func_decl list * ir_func_decl list * (id * int) list
] [@@deriving sexp]

type ir_node = [ir_expr | ir_stmt]

(* variables for recurring constants *)
let ir0 = `IRConst 0L
let ir1 = `IRConst 1L
let ir3 = `IRConst 3L
let ir8 = `IRConst 8L

let string_of_ir_expr e =
  e |> sexp_of_ir_expr |> Sexp.to_string_hum |> String.filter ~f:(fun c -> c <> '\n')
let string_of_ir_stmt s =
  s |> sexp_of_ir_stmt |> Sexp.to_string_hum |> String.filter ~f:(fun c -> c <> '\n')

let ir_expr_of_string str = str |> Sexp.of_string |> ir_expr_of_sexp
let ir_stmt_of_string str = str |> Sexp.of_string |> ir_stmt_of_sexp

let string_of_ir_node n =
  match n with
  | #ir_stmt as s -> string_of_ir_stmt s
  | #ir_expr as e -> string_of_ir_expr e

let rec canonical_of_sexp no_reorder sexp =
  let is_unary s = (s = "IRMem" || s = "IRJump" || s = "IRExp") in
  Sexp.(
    match sexp with
    | List [(Atom "IRCompUnit" as a); List [id; List sexps1; List sexps2; _]] ->
      List (
        List.concat [
          [canonical_of_sexp no_reorder a; id];
          List.map sexps1 ~f:(canonical_of_sexp no_reorder);
          List.map sexps2 ~f:(canonical_of_sexp no_reorder);
        ]
      )
    | List [(Atom "IRFunc" as a); List [id; (List _ as body); _; _]] ->
      List [
        canonical_of_sexp no_reorder a;
        id;
        canonical_of_sexp no_reorder body
      ]
    | List [Atom "IRBinary"; List sexps] ->
      List (List.map sexps ~f:(canonical_of_sexp no_reorder))
    | List [(Atom "IRCall" as a); List [id; List sexps; _; _]] ->
      let a = canonical_of_sexp no_reorder a in
      let id = canonical_of_sexp no_reorder id in
      let sexps = List.map sexps ~f:(canonical_of_sexp no_reorder) in
      List (a :: id :: sexps)
    | List [(Atom "IRCJump" as a); List [e; t; _]] when not no_reorder ->
      let a = canonical_of_sexp no_reorder a in
      let e = canonical_of_sexp no_reorder e in
      let t = canonical_of_sexp no_reorder t in
      List [a; e; t]
    | List [(Atom s as a); List sexps] when not (is_unary s) ->
      let a = canonical_of_sexp no_reorder a in
      let sexps = List.map sexps ~f:(canonical_of_sexp no_reorder) in
      List (a :: sexps)
    | List sexps -> List (List.map sexps ~f:(canonical_of_sexp no_reorder))
    | Atom s ->
      Atom (
        match String.chop_prefix ~prefix:"IR" s with
        | None -> s
        | Some s -> (
            match String.chop_prefix ~prefix:"_" s with
            | None -> String.uppercase s
            | Some s -> String.uppercase s
          )
      )
  )

let canonical_of_lir no_reorder lir =
  lir
  |> sexp_of_ir_comp_unit
  |> canonical_of_sexp no_reorder
  |> Sexp.to_string_hum
  |> (fun in_ ->
      String.Search_pattern.(
        replace_all (create "(SEQ)")
          ~with_:""
          ~in_
      )
    )
  |> (fun in_ ->
      String.Search_pattern.(
        replace_all (create "()")
          ~with_:""
          ~in_
      )
    )


(* Add uses of an expr to a set *)
let rec add_uses_of_expr set (e : ir_expr) =
  match e with
  | `IRTemp _ ->
    Poly_set.add set (string_of_ir_expr e)
  | `IRConst _ -> ()
  | `IRBinary (_, e1, e2) ->
    add_uses_of_expr set e1;
    add_uses_of_expr set e2
  | `IRMem emem ->
    add_uses_of_expr set emem;
  | `IRCall (ef, e_lst, _, _) ->
    add_uses_of_expr set ef;
    List.iter e_lst ~f:(fun e -> add_uses_of_expr set e)
  | `IRName _ -> ()
  (* Impossible cases *)
  | `IRESeq _ -> failwith (Printf.sprintf "Impossible expression: %s" (string_of_ir_expr e))

(* Return the uses of an IR stmt *)
let uses_of_stmt node_data =
  let use_set = empty () in
  (* Update use and def set accordingly *)
  begin match node_data with
    | `Start
    | `Exit  -> ()
    | `Other stmt ->
      begin match stmt with
        | `IRMove (`IRTemp _, e2) ->
          add_uses_of_expr use_set e2
        | `IRMove (`IRMem emem, e2) ->
          add_uses_of_expr use_set emem;
          add_uses_of_expr use_set e2
        | `IRExp e
        | `IRCJump (e, _, _) ->
          add_uses_of_expr use_set e
        | `IRReturn e_lst ->
          List.iter e_lst ~f:(fun e -> add_uses_of_expr use_set e)
        | `IRJump (`IRName _)
        | `IRLabel _ -> ()
        (* Impossible cases *)
        | _ -> failwith (Printf.sprintf "Found impossible statment: %s" (string_of_ir_stmt stmt))
      end
  end;
  (* Return the use set *)
  use_set

let defs_of_stmt node_data =
  let def_set = empty () in
  (* Update def set accordingly *)
  begin match node_data with
    | `Other (`IRMove (`IRTemp _ as t, _)) ->
      Poly_set.add def_set (string_of_ir_expr t);
    | `Start
    | `Exit
    | _ -> ()
  end;
  (* Return the def set *)
  def_set

module IrLocTable = Hash_table.Make(struct
    type t = ir_node
    let equal = phys_equal
    let compare n1 n2 = String.compare (string_of_ir_node n1) (string_of_ir_node n2)
    let hash n = Hash_table.hash (string_of_ir_node n)
    let sexp_of_t n = sexp_of_string (string_of_ir_node n)
    let t_of_sexp _ = (`IRConst (-1L))
  end)

type ir_loc_table = location IrLocTable.t

(* Returns the location (line, col) of a node in the AST *)
let location_of_ir_node tbl (#ir_node as node) =
  match IrLocTable.find tbl node with
  | Some l -> l
  | None -> ("failed in irgen: " ^ string_of_ir_node node, -1, -1)
(* print_locations !locations;
 * failwithf "cannot find location_of_node %s" (string_of_ir_node node) () *)

let ir_add_location tbl n l =
  IrLocTable.set tbl ~key:n ~data:l

let ir_add_locations tbl locs =
  List.iter locs ~f:(fun (n, l) -> ir_add_location tbl n l)

let ir_copy_loc_table ~t_from ~t_to =
  IrLocTable.iteri t_from ~f:(fun ~key ~data -> ir_add_location t_to key data)

let ir_empty_loc_table () = IrLocTable.create ()

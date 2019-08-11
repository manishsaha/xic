open Common
open Core
open Parser

(** Returns string representation of token [t] *)
let string_of_token t = 
  match t with
  | ID id -> "id " ^ id
  | INT i -> "integer " ^ i
  | STRING us -> "string " ^ string_of_uchar_list us
  | CHAR u -> "character " ^ string_of_uchar u
  | BOOL b -> Bool.to_string b
  | TYPE t -> t
  | USE -> "use"
  | IF -> "if"
  | WHILE -> "while"
  | ELSE -> "else"
  | RETURN -> "return"
  | LENGTH -> "length"
  | CLASS -> "class"
  | EXTENDS -> "extends"
  | NEW -> "new"
  | THIS -> "this"
  | BREAK -> "break"
  | NULL -> "null"
  | ASSIGN -> "="
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | UNDERSCORE -> "_"
  | COMMA -> ","
  | COLON -> ":"
  | SEMICOLON -> ";"
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | HIGH_MULT -> "*>>"
  | DIV -> "/"
  | MOD -> "%"
  | EQ -> "=="
  | NEQ -> "!="
  | LEQ -> "<="
  | LT -> "<"
  | GEQ -> ">="
  | GT -> ">"
  | AND -> "&"
  | OR -> "|"
  | NOT -> "!"
  | DOT -> "."
  | EOF -> ""

let print_diagnostic root cout =
  Printf.fprintf cout "%s" (Ast.string_of_root root)

let main parse_func tps =
  let tps_ref = ref tps in
  let tp_ref = ref (List.hd_exn !tps_ref) in
  let tp_func () =
    let tps = !tps_ref in
    let tp = List.hd_exn tps in
    tps_ref := List.tl_exn tps;
    tp_ref := tp;
    tp
  in
  try MenhirLib.Convert.Simplified.traditional2revised parse_func tp_func
  with Parser.Error ->
    let (t, s, _) = !tp_ref in
    let (f, l, c) as err_loc = location_of_position s in
    let err_msg = "unexpected token " ^ string_of_token t in
    Printf.printf "Syntax error beginning at %s:%d:%d: %s\n" f l c err_msg;
    raise (Xic_error ({
        err_type = "Syntax";
        err_msg = err_msg;
        err_loc = err_loc;
      }))

let main_program = main Parser.parse_program
let main_interface = main Parser.parse_interface

open Common
open Core
open Lexing
open Parser
open Sedlexing

module Uchar = Base.Uchar

(** Lexer metadata/functions *)
type metadata = {
  mutable token: token;
  mutable location: location;
  mutable string_chars: escaped_char list list;
  mutable string_start: int;
  mutable in_string: bool;
}

let metadata = {
  token = Parser.EOF;
  location = ("", 0, 0);
  string_chars = [];
  string_start = -1;
  in_string = false;
}

let line_num lexbuf =
  let (s, _) = lexing_positions lexbuf in s.pos_lnum
let col_num lexbuf =
  let (s, _) = lexing_positions lexbuf in s.pos_cnum - s.pos_bol + 1

(** Char/string lexing utils *)
let lexeme_string lexbuf ?start:(start=0) ?len:(len=lexeme_length lexbuf) () =
  let us = sub_lexeme lexbuf start len in
  us
  |> Array.to_list
  |> List.map ~f:(fun u -> u |> Uchar.to_char_exn |> Char.to_string)
  |> String.concat

let update lexbuf token =
  let (f, l, c) = location_of_position (fst (lexing_positions lexbuf)) in
  let c =
    match token with
    | STRING _ -> metadata.string_start
    | EOF when metadata.in_string -> metadata.string_start
    | _ -> c in
  metadata.location <- (f, l, c);
  metadata.token <- token

let lexing_positions lexbuf =
  let (s, e) = lexing_positions lexbuf in
  let (_, _, c) = metadata.location in
  ({s with pos_cnum = c + s.pos_bol - 1}, e)

(** Char/string lexing utils *)
let add_chars cs = metadata.string_chars <- cs :: metadata.string_chars

let get_string () =
  metadata.in_string <- false;
  metadata.string_chars |> List.rev |> List.concat

let init_string lexbuf =
  metadata.string_chars <- [];
  metadata.string_start <- col_num lexbuf;
  metadata.in_string <- true

let raise_lexical_error lexbuf err_msg =
  update lexbuf EOF;
  let (f, l, c) as err_loc = metadata.location in
  Printf.printf "Lexical error beginning at %s:%d:%d: %s\n" f l c err_msg;
  raise (Xic_error ({
      err_type = "Lexical";
      err_msg = err_msg;
      err_loc = err_loc;
    }))

(** Lexing rules *)
let uchars_of_escape_chars =
  List.map ~f:(fun (c, i) -> (Uchar.of_char c, Uchar.of_scalar_exn i))
    [
      ('n', 0xA);
      ('r', 0xD);
      ('b', 0x8);
      ('t', 0x9);
    ]

let special_char = [%sedlex.regexp? '\\' | '\'' | '"']
let escape_char = [%sedlex.regexp? 'n' | 'r' | 'b' | 't' | special_char]
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ]
let escape_hex = [%sedlex.regexp? Rep (hex_digit, 1 .. 4)]

let rec token' lexbuf =
  match%sedlex lexbuf with
  | ' ' | '\t' -> token' lexbuf
  | Opt '\r', '\n' -> token' lexbuf
  | "use" -> USE
  | "if" -> IF
  | "while" -> WHILE
  | "else" -> ELSE
  | "return" -> RETURN
  | "length" -> LENGTH
  | "class" -> CLASS
  | "extends" -> EXTENDS
  | "new" -> NEW
  | "this" -> THIS
  | "break" -> BREAK
  | "null" -> NULL
  | "=" -> ASSIGN
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "[" -> LBRACKET
  | "]" -> RBRACKET
  | "{" -> LBRACE
  | "}" -> RBRACE
  | "_" -> UNDERSCORE
  | "," -> COMMA
  | ":" -> COLON
  | ";" -> SEMICOLON
  | "+" -> PLUS
  | "-" -> MINUS
  | "*>>" -> HIGH_MULT
  | "*" -> MULT
  | "/" -> DIV
  | "%" -> MOD
  | "==" -> EQ
  | "!=" -> NEQ
  | "<=" -> LEQ
  | "<" -> LT
  | ">=" -> GEQ
  | ">" -> GT
  | "&" -> AND
  | "|" -> OR
  | "!" -> NOT
  | '\'', '\\', 'x', escape_hex, '\'' ->
    let h =
      "0" ^ lexeme_string lexbuf ~start:2 ~len:(lexeme_length lexbuf - 3) ()
    in
    CHAR (h |> Int.of_string |> Uchar.of_scalar_exn)
  | '\'', '\\', escape_char, '\'' ->
    let u = (lexeme lexbuf).(2) in
    let u =
      try List.Assoc.find_exn uchars_of_escape_chars u ~equal:(=)
      with _ -> u
    in
    CHAR u
  | '\'', Compl ('\'' | '\\' | '\n'), '\'' ->
    CHAR (lexeme lexbuf).(1)
  | '1' .. '9', Star '0' .. '9' | '0' ->
    let s = lexeme_string lexbuf () in
    if s = "9223372036854775808" && metadata.token = MINUS
    then INT s
    else
      begin
        try Int64.of_string s |> ignore; INT s
        with _ -> raise_lexical_error lexbuf "invalid integer literal"
      end
  | "." -> DOT
  | "true" -> BOOL true
  | "false" -> BOOL false
  | "bool" | "int" -> TYPE (lexeme_string lexbuf ())
  | alphabetic, Star (alphabetic | '0'..'9' | '_' | '\'') ->
    ID (lexeme_string lexbuf ())
  | '\"' -> init_string lexbuf; string lexbuf
  | "//", Star (Compl '\n') -> token' lexbuf
  | '\'', '\n', '\'' -> raise_lexical_error lexbuf "invalid character literal"
  | '\'', '\'' -> raise_lexical_error lexbuf "empty character literal"
  | eof -> EOF
  | _ -> raise_lexical_error lexbuf "invalid token"

and string lexbuf =
  match%sedlex lexbuf with
  | '"' -> STRING (get_string ())
  | '\\', 'x', escape_hex ->
    let
      h = "0" ^ lexeme_string lexbuf ~start:1 ~len:(lexeme_length lexbuf - 1) ()
    in
    add_chars [h |> Int.of_string |> Uchar.of_scalar_exn];
    string lexbuf
  | '\\', escape_char ->
    let u = (lexeme lexbuf).(1) in
    let u =
      try List.Assoc.find_exn uchars_of_escape_chars u ~equal:(=)
      with _ -> u
    in
    add_chars [u];
    string lexbuf
  | '\\', special_char
  | '\''
  | Star (Compl (special_char | '\n')) ->
    lexeme lexbuf |> Array.to_list |> add_chars;
    string lexbuf
  | special_char
  | '\n' -> raise_lexical_error lexbuf "invalid string literal"
  | _ -> raise_lexical_error lexbuf "invalid string literal"

let token lexbuf =
  try let t = token' lexbuf in update lexbuf t; t
  with e -> update lexbuf EOF; raise e

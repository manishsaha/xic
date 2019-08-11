%{
  open Ast
  open Common
  open Core

  let locations = ref []

  let add (node : node) position =
    locations := List.Assoc.add !locations node (location_of_position position) ~equal:phys_equal
%}

%token EOF
%token <string> ID
%token <string> INT
%token <Common.escaped_char list> STRING
%token <Common.escaped_char> CHAR
%token <bool> BOOL
%token <string> TYPE
%token USE
%token NEW
%token DOT
%token IF
%token WHILE
%token ELSE
%token RETURN
%token LENGTH
%token CLASS
%token EXTENDS
%token THIS
%token BREAK
%token NULL
%token ASSIGN
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token UNDERSCORE
%token COMMA
%token COLON
%token SEMICOLON
%token PLUS
%token MINUS
%token MULT
%token HIGH_MULT
%token DIV
%token MOD
%token EQ
%token NEQ
%token LEQ
%token LT
%token GEQ
%token GT
%token AND
%token OR
%token NOT

%nonassoc IF
%nonassoc ELSE

%left OR
%left AND
%left EQ NEQ
%left LT LEQ GEQ GT
%left PLUS MINUS
%left MULT HIGH_MULT DIV MOD
%nonassoc NOT
/* %left DOT */
/* %left LBRACKET */

%type <[> Ast.var_decl ]> var_decl
%type <[> Ast.var_init ]> var_init

%type <Ast.program_member> program_member

%start <Ast.interface * Ast.locations> parse_interface
%start <Ast.program * Ast.locations> parse_program

%%

parse_interface:
  | us = list(use); e = interface_body; EOF
    {
      let (ms, cs) = e in
      let n = `Interface (us, ms, cs) in
      add n $startpos;
      (n, !locations)
    }

interface_body:
  | { ([], []) }
  | m = meth_sig; e = interface_body
    { let (ms, cs) = e in (m :: ms, cs) }
  | c = class_sig; e = interface_body
    { let (ms, cs) = e in (ms, c :: cs) }

parse_program:
  | uses = list(use); members = nonempty_list(program_member); EOF
    {
      let (globals, meths, classes) =
        List.fold_right members ~init:([], [], []) ~f:(fun member (gs, ms, cs) ->
            match member with
            | `Meth _ as m -> (gs, m :: ms, cs)
            | `Class _ as c -> (gs, ms, c :: cs)
            | `VarDecl _
            | `VarInit _ as g -> (g :: gs, ms, cs)
          )
      in
      let n = `Program (uses, globals, meths, classes) in
      add n $startpos;
      (n, !locations)
    }

program_member:
  | c = xi_class
    { c }
  | m = meth
    { (m :> program_member) }
  | g = global
    { g }

global:
  | s = var_decl
  | s = var_init
    { s }

xi_class:
  | CLASS; id = ID; EXTENDS; ex = ID; LBRACE; members = list(class_member); RBRACE
    {
      let (ds, ms) =
        List.fold_right members ~init:([], []) ~f:(fun member (ds, ms) ->
            match member with
            | `VarDecl ds' -> (ds @ ds', ms)
            | `Meth _ as m -> (ds, m :: ms)
          )
        |> (fun (ds, ms) -> (ds, List.rev ms))
      in
      let n = `Class (id, Some ex, ds, ms) in
      add n $startpos;
      n
    }
  | CLASS; id = ID; LBRACE; members = list(class_member); RBRACE
    {
      let (ds, ms) =
        List.fold_right members ~init:([], []) ~f:(fun member (ds, ms) ->
            match member with
            | `VarDecl ds' -> (ds @ ds', ms)
            | `Meth _ as m -> (ds, m :: ms)
          )
        |> (fun (ds, ms) -> (ds, List.rev ms))
      in
      let n = `Class (id, None, ds, ms) in
      add n $startpos;
      n
    }

class_member:
  | d = var_decl
    { d }
  | m = meth
    { m }

class_sig:
  | CLASS; id = ID; EXTENDS; ex = ID; LBRACE; msigs = list(meth_sig); RBRACE
    {
      let n = `ClassSig (id, Some ex, msigs) in
      add n $startpos;
      n
    }
  | CLASS; id = ID; LBRACE; msigs = list(meth_sig); RBRACE
    {
      let n = `ClassSig (id, None, msigs) in
      add n $startpos;
      n
    }

use:
  | USE; id = ID; SEMICOLON?
    {
      let n = `Use id in
      add n $startpos(id);
      n
    }

nonarray_type:
  | t = TYPE
    { if t = "int" then `IntAnnot () else `BoolAnnot () }
  | t = ID
    { `ClassAnnot t }

brackets_list:
  | nones = list(LBRACKET; RBRACKET { None })
    { nones }
  | LBRACKET; e = expr; RBRACKET; rest = brackets_list
    { Some e :: rest }

decl_type:
  | t = nonarray_type; e_opts = brackets_list
    { List.fold_right e_opts ~init:t ~f:(fun e_opt t' -> `ArrayAnnot (t', e_opt)) }

init_type:
  | t = nonarray_type; nones = list(LBRACKET; RBRACKET { None })
    { List.fold_right nones ~init:t ~f:(fun _ t' -> `ArrayAnnot (t', None)) }

multi_decl:
  | ids = separated_nonempty_list(COMMA, ID); COLON; t = decl_type
    {
      List.map ids ~f:(fun id ->
          let n = `Decl (id, t) in
          add n $startpos;
          n
        )
    }

init:
  | ids = separated_nonempty_list(COMMA, ID); COLON; t = init_type
    {
      List.map ids ~f:(fun id ->
          let n = `Decl (id, t) in
          add n $startpos;
          n
        )
    }
  | UNDERSCORE
    {
      let n = `Discard () in
      add n $startpos;
      [n]
    }

param_type:
  | t = TYPE
    { if t = "int" then `IntAnnot () else `BoolAnnot () }
  | t = param_type; LBRACKET; RBRACKET
    { `ArrayAnnot (t, None) }
  | t = ID
    { `ClassAnnot t }

param:
  | id = ID; COLON; t = param_type
    {
      let n = `Param (id, t) in
      add n $startpos;
      n
    }

meth:
  | id = ID; LPAREN; ds = separated_list(COMMA, param); RPAREN; COLON; ts = separated_nonempty_list(COMMA, param_type); b = block
    {
      let n = `Meth (id, ds, ts, b) in
      add n $startpos;
      n
    }
  | id = ID; LPAREN; ds = separated_list(COMMA, param); RPAREN; b = block
    {
      let n = `Meth (id, ds, [], b) in
      add n $startpos;
      n
    }

meth_sig:
  | id = ID; LPAREN; ds = separated_list(COMMA, param); RPAREN; COLON; ts = separated_nonempty_list(COMMA, param_type); SEMICOLON?
    {
      let n = `MethSig (id, ds, ts) in
      add n $startpos;
      n
    }
  | id = ID; LPAREN; ds = separated_list(COMMA, param); RPAREN; SEMICOLON?
    {
      let n = `MethSig (id, ds, []) in
      add n $startpos;
      n
    }

array_expr:
  | LBRACE; es = flexible_list(COMMA, expr); RBRACE
    {
      let n = `Array es in
      add n $startpos;
      n
    }
  | e1 = array_expr; LBRACKET; e2 = expr; RBRACKET
    {
      let n = `Binary (`Index, e1, e2) in
      add n $startpos;
      n
    }

var_expr: 
  | id = ID
    {
      let n = `Var id in
      add n $startpos;
      n
    }
  | e = var_expr; LPAREN; es = separated_list(COMMA, expr); RPAREN
    {
      let n = `ECall (e, es) in
      add n $startpos;
      n
    }
  | e1 = var_expr; LBRACKET; e2 = expr; RBRACKET
    {
      let n = `Binary (`Index, e1, e2) in
      add n $startpos;
      n
    }
  | e = dot_expr
    { e }

obj_expr:
  | e = var_expr
    { e }
  | NEW; c_id = ID
    {
      let n1 = `Var c_id in
      let n2 = `Unary (`New, n1) in
      add n1 $startpos(c_id);
      add n2 $startpos;
      n2
    }
  | NULL
    { 
      let n = `Null () in
      add n $startpos;
      n
    }
  | THIS
    { 
      let n = `This () in
      add n $startpos;
      n
    }

dot_expr:
  | e = obj_expr; DOT; id = ID
    {
      let n = `Binary (`Dot, e, `Var id) in
      add n $startpos($2);
      n
    }

expr:
  | e = obj_expr
  | e = array_expr
  | LPAREN; e = expr; RPAREN
    { e }
  | i = INT
    {
      let n = `Int (Bigint.of_string i) in
      add n $startpos;
      n
    }
  | b = BOOL
    { 
      let n = `Bool b in
      add n $startpos;
      n
    }
  | s = STRING
    { 
      let n = `String s in
      add n $startpos;
      n
    }
  | c = CHAR
    { 
      let n = `Char c in
      add n $startpos;
      n
    }
  | LENGTH; LPAREN; e = expr; RPAREN
    { 
      let n = `Length e in
      add n $startpos;
      n
    }
  | uop = unop; e = expr %prec NOT
    { 
      let n = `Unary (uop, e) in
      add n $startpos;
      n
    }
  | e1 = expr; bop = binop; e2 = expr
    { 
      let n = `Binary (bop, e1, e2) in
      add n $startpos(bop);
      n
    }

/* x, y: int */
var_decl:
  | d = multi_decl; SEMICOLON?
    { 
      let n = `VarDecl d in
      add n $startpos;
      n
    }

/* x: int, _, z: int = f(5) */
var_init:
  | ds = separated_nonempty_list(COMMA, init); ASSIGN; e = expr; SEMICOLON?
    { 
      let n = `VarInit (List.concat ds, e) in
      add n $startpos($2);
      n
    }

stmt:
  | e1 = var_expr; ASSIGN; e2 = expr; SEMICOLON?
    { 
      let n = `Assign (e1, e2) in
      add n $startpos($2);
      n
    }
  | s = var_decl
  | s = var_init
    { s }
  | IF; e = expr; b = stmt %prec IF
    { 
      let n = `If (e, b) in
      add n $startpos;
      n
    }
  | IF; e = expr; b1 = stmt; ELSE; b2 = stmt
    { 
      let n = `IfElse (e, b1, b2) in
      add n $startpos;
      n
    }
  | WHILE; e = expr; b = stmt
    {
      let n = `While (e, b) in
      add n $startpos;
      n
    }
  | ef = var_expr; LPAREN; es = separated_list(COMMA, expr); RPAREN; SEMICOLON?
    { 
      let n = `SCall (ef, es) in
      add n $startpos;
      n
    }
  | b = block; SEMICOLON?
    { (b :> stmt) }
  | BREAK; SEMICOLON?
    { 
      let n = `Break () in
      add n $startpos;
      n
    }

block:
  | LBRACE; b = list(stmt); r = return?; RBRACE
    {
      let n = `Block (b, r) in
      add n $startpos;
      n
    }

return:
  | RETURN; es = separated_list(COMMA, expr); SEMICOLON?
    { 
      let n = `Return es in
      add n $startpos;
      n
    }

%inline unop:
  | MINUS { `Negate }
  | NOT   { `Not }

%inline binop:
  | OR        { `Or }
  | AND       { `And }
  | EQ        { `Eq }
  | NEQ       { `Neq }
  | LEQ       { `Leq }
  | LT        { `Lt }
  | GEQ       { `Geq }
  | GT        { `Gt }
  | PLUS      { `Plus }
  | MINUS     { `Minus }
  | MULT      { `Mult }
  | HIGH_MULT { `HighMult }
  | DIV       { `Div }
  | MOD       { `Mod }

/* allow for trailing commas, from menhir docs */
%inline flexible_list(delim, X):
  | xs = separated_llist(delim, X) delim?
    { xs }

reverse_separated_nonempty_llist(separator, X):
  | x = X
    { [ x ] }
  | xs = reverse_separated_nonempty_llist(separator, X); separator; x = X
    { x :: xs }

%inline reverse_separated_llist(separator, X):
  | { [] }
  | xs = reverse_separated_nonempty_llist(separator, X)
    { xs }

%inline separated_llist(separator, X):
  | xs = reverse_separated_llist(separator, X)
    { List.rev xs }

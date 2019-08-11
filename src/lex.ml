open Common
open Core

let tps_ref = ref []

let print_diagnostic tps cout =
  List.iter tps ~f:(fun (t, s, _) ->
      let (_, l, c) = location_of_position s in
      if t <> Parser.EOF then
        Printf.fprintf cout "%d:%d %s\n" l c (Parse.string_of_token t)
    )

let print_error l c err_msg cout =
  print_diagnostic (List.rev !tps_ref) cout |> ignore;
  Printf.fprintf cout "%d:%d error: %s\n" l c err_msg

(** Lexes input from [src] file to designated [dst] file *)
let main src =
  let cin = In_channel.create src in
  let lexbuf = Sedlexing.Utf8.from_channel cin in
  Sedlexing.set_filename lexbuf src;
  tps_ref := [];
  let loop = ref true in
  while !loop do
    let t = Lexer.token lexbuf in
    let (s, e) = Lexer.lexing_positions lexbuf in
    tps_ref := (t, s, e) :: !tps_ref;
    if t = Parser.EOF
    then loop := false
  done;
  In_channel.close cin;
  List.rev !tps_ref

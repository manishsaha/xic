open Common
open Core

(* Return extension of diagnostic file based on diagnostic flag *)
let ext_of_flag flag =
  match flag with
  | "lex" -> ".lexed"
  | "parse" -> ".parsed"
  | "typecheck" -> ".typed"
  | "irgen" -> ".ir"
  | "irrun" -> ".ir"
  | "print_mir" -> ".mir"
  | "asm" -> ".s"
  | _ -> failwith (Printf.sprintf "invalid flag found: %s" flag)

(* From a list of files and their source path, return all the ones that
 * exist and are valid Xi files *)
let get_valid_files files src_path =
  List.filter files ~f:(fun file ->
      let valid = get_ext file |> valid_ext in
      if not valid then
        printf
          "Warning: File '%s' has an invalid extension. \
           Skipping this file\n"
          file;
      valid
    )
  |> List.filter ~f:(fun file ->
      match Sys.is_file (Filename.concat src_path file) with
      | `Yes -> true
      | _ ->
        printf
          "Warning: File '%s' couldn't be found. Skipping this file\n" file;
        false
    )

(* Given existing xi files [files] and a [src_path] to find them, return
 * a list of file paths representing that file *)
let get_in_paths files src_path =
  List.map files ~f:(Filename.concat src_path)

(* Return a list of pairs ([p1], [p2]), where
   - [p1] is a file to output diagnostics to, relative to [out_path].
   - [p2] is a file to output assembly to, relative to [asm_path]
*)
let get_output_paths in_files out_path asm_path =
  List.map in_files ~f:(fun in_file ->
      let base_file = remove_ext in_file in
      let diag_out_path = Filename.concat out_path base_file in
      let asm_out_path = Filename.concat asm_path base_file in
      Unix.mkdir_p (Filename.dirname diag_out_path);
      Unix.mkdir_p (Filename.dirname asm_out_path);
      diag_out_path, asm_out_path
    )

(* Creaate a mapping that tells where to output diagnostic files based on the
   flag/phase *)
let make_diag_info diag_flags diag_out_path asm_out_path =
  List.map diag_flags ~f:(fun (flag, active) ->
      let out_file =
        if not active then
          "/dev/null"
        else if flag = "asm" then
          asm_out_path ^ (ext_of_flag flag)
        else
          diag_out_path ^ (ext_of_flag flag)
      in
      (flag, (out_file, `Unvisited))
    )
  |> Hash_table.of_alist_exn (module String)

(** Get the file to output diagnostics based on the flag. Any optimization
    flag (i.e. reg, cse, etc.) won't have the entire file specified because
    the output depends on what function is being run.
*)
let file_of_flag diag_info flag =
  Hash_table.find_exn diag_info flag |> fst

let get_flag_info diag_info flag =
  Hash_table.find_exn diag_info flag

(* Run [func], passing to it an output channel to [file] *)
let execute_with_channel (module Config : ConfigSpec) func flag_opt file =
  let open Config in
  Option.iter flag_opt ~f:(fun flag -> Hash_table.set diag_info ~key:flag ~data:(file, `Visited));
  let cout = Out_channel.create file in
  func cout;
  Out_channel.close cout

(* Perform all stages of compilation of a .xi file [base_file], using the
   tokens [token_ps], getting library files from [libs], and writing diagnostic
   output based on [diag_order].
   Various other flags are passed for specific behavior
*)
let compile_xi_file ~token_ps ~base_file (module Config : ConfigSpec) =
  let open Config in
  (* Parse the program and print diagnostics *)
  let (prog, ast_locs) = Parse.main_program token_ps in
  execute_with_channel
    (module Config)
    (Parse.print_diagnostic prog)
    (Some "parse")
    (file_of_flag diag_info "parse");

  (* Typecheck the program and print diagnostics *)
  let ((prog, ast_locs), global_ctx) : (Ast.program * Ast.loc_table) * Ast.context =
    prog
    |> Typecheck.main_program libs ast_locs base_file
    |> Mangle.main
  in
  execute_with_channel
    (module Config)
    Typecheck.print_diagnostic
    (Some "typecheck")
    (file_of_flag diag_info "typecheck");

  (* Do uninitialized variable analysis *)
  (* Uninit_dfa.main (module Config) locations prog |> ignore; *)

  (* Generate MIR and LIR. Print the MIR *)
  let (mir, lir, ir_locs) =
    (prog, ast_locs)
    |> Const_fold.main_ast (module Config)
    |> Irgen.main base_file global_ctx in
  execute_with_channel
    (module Config)
    (Irgen.print_mir_diagnostic mir)
    (Some "print_mir")
    (file_of_flag diag_info "print_mir");

  (* Generate canonical IR and print diagnostics *)
  let ir =
    lir
    |> Reorder.main no_reorder
  in

  (* Print IR before any optimization if specified *)
  if Hash_table.find_exn Config.optir_flags "initial" then
    execute_with_channel
      (module Config)
      (Irgen.print_diagnostic no_reorder ir)
      None
      (Config.diag_out_path ^ "_initial.ir");

  let ir =
    ir
    |> Uninit_dfa.main (module Config) ir_locs
    |> Const_fold.main_ir (module Config)
    |> Copy_propagate.main (module Config)
    |> Dead_code_elim.main (module Config)
    |> Cse.main (module Config)
  in
  execute_with_channel
    (module Config)
    (Irgen.print_diagnostic no_reorder ir)
    (Some "irgen")
    (file_of_flag diag_info "irgen");

  (* Print IR after any optimization if specified *)
  if Hash_table.find_exn Config.optir_flags "final" then
    execute_with_channel
      (module Config)
      (Irgen.print_mir_diagnostic ir)
      None
      (Config.diag_out_path ^ "_final.ir");

  (* Run the intepreter if specified by the command *)
  let ir_file = file_of_flag diag_info "irrun" in
  if irrun then (
    Sys.command
      ("java -cp src/interpreter/java_cup.jar:src/interpreter/build/classes \
        edu.cornell.cs.cs4120.xic.ir.interpret.Main "
       ^ ir_file)
    |> ignore;
    if not irgen then Sys.command ("rm " ^ ir_file) |> ignore
  );
  (* Generate assembly *)
  let x86 = Assemble.main ir (module Config) in
  execute_with_channel
    (module Config)
    (Assemble.print_assembly x86)
    (Some "asm")
    (file_of_flag diag_info "asm")

(* Perform all stages of compilation of a .ixi file using the
   tokens [token_ps]
*)
let compile_ixi_file ~token_ps (module Config : ConfigSpec) =
  let open Config in
  (* Parse program *)
  let (int, locations) = Parse.main_interface token_ps in
  execute_with_channel
    (module Config)
    (Parse.print_diagnostic int)
    (Some "parse")
    (file_of_flag diag_info "parse");

  (* Typecheck program *)
  let _ = Typecheck.main_interface libs locations int in
  execute_with_channel
    (module Config)
    Typecheck.print_diagnostic
    (Some "typecheck")
    (file_of_flag diag_info "typecheck")

(* Perform all stages of compilation for the file [in_path].
   Uses library files in the path [libs].
   Outputs diagnostics based on which flags in [diag_flags] are set
   Various other flags are passed for specific behavior
*)
let compile_file (module Config : ConfigSpec) =
  let open Config in
  try
    (* Lex the program and print diagnostics *)
    let base_file = in_path |> base_filename |> remove_ext in
    let token_ps = Lex.main in_path in
    execute_with_channel
      (module Config)
      (Lex.print_diagnostic token_ps)
      (Some "lex")
      (file_of_flag diag_info "lex");

    (* Perform the rest of compilation on the program *)
    if get_ext in_path = "xi" then (
      compile_xi_file ~token_ps ~base_file (module Config)
    ) else (
      compile_ixi_file ~token_ps (module Config)
    )
  with Xic_error {err_msg; err_loc = (_, l, c); _} ->
    (* If an error occurred somewhere during compilation, propagate
       the errors to the rest of the diagnostic files *)
    Hash_table.iteri diag_info ~f:(fun ~key:flag ~data:(filename, visited) ->
        if visited = `Unvisited then
          let cout = Out_channel.create filename in
          if flag = "lex" then
            Lex.print_error l c err_msg cout
          else if not (List.mem _SUPPORTED_OPTS_ flag ~equal:(=)) then
            Printf.fprintf cout "%d:%d error: %s\n" l c err_msg;
          Out_channel.close cout
      )

(* Create a command that can be run with various flags *)
let rec generate_command () =
  Command.basic
    ~summary: "Description: A compiler for the 'Xi' language"
    Command.Let_syntax.
      (let%map_open help = flag "--help" no_arg
           ~doc:" print a synopsis of options"

       and out_dir = flag "-D" (optional string)
           ~doc:" specify where to place generated diagnostic files"

       and asm_dir = flag "-d" (optional string)
           ~doc:" Specify where to place generated assembly output files"

       and src_dir = flag "-sourcepath" (optional string)
           ~doc:" specify where to find input source files"

       and lib_dir = flag "-libpath" (optional string)
           ~doc:" specify where to find library interface files"

       and lex = flag "--lex" no_arg
           ~doc:" generate output from lexical analysis"

       and parse = flag "--parse" no_arg
           ~doc:" generate output from lexical analysis"

       and typecheck = flag "--typecheck" no_arg
           ~doc:" generate output from semantic analysis"

       and irgen = flag "--irgen" no_arg
           ~doc:" generate intermediate code"

       and irrun = flag "--irrun" no_arg
           ~doc:" generate and interpret intermediate code"

       and _ = flag "-target" (optional string)
           ~doc:" Specify the operating system for which to generate code"

       and no_reorder = flag "--no-reorder" no_arg
           ~doc:" disables block reordering"

       and print_mir = flag "--print-mir" no_arg
           ~doc:" outputs MIR"

       and report_opts = flag "--report-opts" no_arg
           ~doc:" output (only) a list of optimizations supported by the compiler"

       and optir = flag "--optir" (listed string)
           ~doc:" report the intermediate code at the specified phase of optimization"

       and optcfg = flag "--optcfg" (listed string)
           ~doc:" report the control-flow graph at the specified phase of optimization"

       and disable_all_opts = flag "-O" no_arg
           ~doc:" disable all optimizations"

       and o_cf = flag "-Ocf" no_arg
           ~doc:" enable constant folding"

       and o_reg = flag "-Oreg" no_arg
           ~doc:" enable register allocation (and move coalescing)"

       and o_mc = flag "-Omc" no_arg
           ~doc:" enable move coalescing (and register allocation)"

       and o_cse = flag "-Ocse" no_arg
           ~doc:" enable common subexpression elimination"

       and o_copy = flag "-Ocopy" no_arg
           ~doc:" enable copy propagation"

       and o_dce = flag "-Odce" no_arg
           ~doc:" enable dead code elimination"

       and o_no_cf = flag "-O-no-cf" no_arg
           ~doc:" disable constant folding"

       and o_no_reg = flag "-O-no-reg" no_arg
           ~doc:" disable register allocation (and move coalescing)"

       and o_no_mc = flag "-O-no-mc" no_arg
           ~doc:" disable move coalescing (and register allocation)"

       and o_no_cse = flag "-O-no-cse" no_arg
           ~doc:" disable common subexpression elimination"

       and o_no_copy = flag "-O-no-copy" no_arg
           ~doc:" disable copy propagation"

       and o_no_dce = flag "-O-no-dce" no_arg
           ~doc:" disable dead code elimination"

       and noextension = flag "-noextension" no_arg
           ~doc:" raise errors when there are variables which are definitely uninitialized"

       and debug = flag "-debug" (optional int)
           ~doc:" set debug level"

       and files = anon (sequence ("filename" %: string)) in

       fun () ->
         if help then (
           run_helper ()
         ) else if report_opts then (
           List.iter _SUPPORTED_OPTS_ ~f:print_endline
         ) else if files = [] then (
           printf "No files were passed to compile...\n";
           run_helper ()
         ) else (
           Common._DEBUG_ := Option.value debug ~default:0;
           let out_path = make_path out_dir in
           let src_path = make_path src_dir in
           let lib_path = make_path lib_dir in
           let asm_path = make_path asm_dir in
           let valid_files = get_valid_files files src_path in
           let in_paths = get_in_paths valid_files src_path in
           let libs = get_libs lib_path in
           let output_paths = get_output_paths valid_files out_path asm_path in
           (* Specify which diagnostics are active *)
           let diag_flags = [
             ("lex",        lex);
             ("parse",      parse);
             ("typecheck",  typecheck);
             ("print_mir",  print_mir);
             ("irgen",      irgen || irrun);
             ("irrun",      irrun);
             ("asm",        true);
           ] in

           (* Map of which IR should be reported per phase *)
           let optir_flags = Hash_table.of_alist_exn (module String) [
               ("initial", List.mem optir "initial" ~equal:(=));
               ("final",   List.mem optir "final" ~equal:(=));
             ] in

           (* Map of which CFG should be reported per phase *)
           let optcfg_flags = Hash_table.of_alist_exn (module String) [
               ("cf",   List.mem optcfg "cf" ~equal:(=));
               ("reg",  List.mem optcfg "reg" ~equal:(=));
               ("mc",   List.mem optcfg "mc" ~equal:(=));
               ("cse",  List.mem optcfg "cse" ~equal:(=));
               ("copy", List.mem optcfg "copy" ~equal:(=));
               ("dce",  List.mem optcfg "dce" ~equal:(=));
             ] in

           (* Map of optimizations that were specified to be enabled *)
           let opt_flags = Hash_table.of_alist_exn (module String) [
               ("cf",   o_cf);
               ("reg",  o_reg);
               ("mc",   o_mc);
               ("cse",  o_cse);
               ("copy", o_copy);
               ("dce",  o_dce);
             ] in

           (* An optimization <opt> is considered "enabled" only when:
              - If -O or, for some <opt'>, -O<opt'> is specified,
                then -O<opt> must be specified
              - Otherwise, -O-no-<opt> must NOT be specified
           *)
           let enabled_opts =
             let opts_initially_off = Hash_table.fold opt_flags
                 ~init:disable_all_opts
                 ~f:(fun ~key:_ ~data:enabled acc -> acc || enabled)
             in
             let is_enabled o o_no = if opts_initially_off then o else not o_no in
             Hash_table.of_alist_exn (module String) [
               ("cf",   is_enabled o_cf   o_no_cf);
               ("reg",  is_enabled o_reg  o_no_reg);
               ("mc",   is_enabled o_mc   o_no_mc);
               ("cse",  is_enabled o_cse  o_no_cse);
               ("copy", is_enabled o_copy o_no_copy);
               ("dce",  is_enabled o_dce  o_no_dce);
             ]
           in

           List.iter2_exn in_paths output_paths
             ~f:(fun in_path (diag_out_path, asm_out_path) ->
                 let diag_info = make_diag_info diag_flags diag_out_path asm_out_path in
                 let module Config : ConfigSpec = struct
                   let in_path = in_path
                   let diag_out_path = diag_out_path
                   let asm_out_path = asm_out_path
                   let diag_flags = diag_flags
                   let diag_info = diag_info
                   let libs = libs
                   let disable_all_opts = disable_all_opts
                   let no_reorder = no_reorder
                   let irgen = irgen
                   let irrun = irrun
                   let optir_flags = optir_flags
                   let optcfg_flags = optcfg_flags
                   let enabled_opts = enabled_opts
                   let noextension = noextension
                 end in
                 compile_file (module Config)
               )
         )
      )

(* Helper to run command which prints out a synopsis of options *)
and run_helper () =
  Command.run
    ~extend:(fun _ -> ["-help"])
    (generate_command ())

(* MAIN PROGRAM *)
let () =
  Command.run (generate_command ())

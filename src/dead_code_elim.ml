open Cfg
open Common
open Core
open Graph_lib
open Ir

(* Get all IRTemps with _ARG or _RET prefix from set *)
let all_args_rets set =
  let args_rets = empty () in
  Poly_set.iter set ~f:(fun var ->
      if String.is_prefix var ~prefix:"(IRTemp _ARG" ||
         String.is_prefix var ~prefix:"(IRTemp _RET" then
        Poly_set.add args_rets var
    );
  Poly_set.iter args_rets ~f:(Poly_set.remove set)

let top_func () = empty ()
let meet = Poly_set.union

let transfer {node_data; _} l =
  (* F_n(l) = use(n) U (l - def(n)) *)
  let uses = uses_of_stmt node_data in
  let defs = defs_of_stmt node_data in
  Poly_set.(union uses (diff l defs))

let display cfg file =
  let cout = Out_channel.create file in
  DDot.output_graph cout
    (ocamlgraph_of_graph cfg
       (fun {node_data; _} ->
          match node_data with
          | `Start -> "START"
          | `Exit -> "EXIT"
          | `Other stmt -> String.strip (string_of_ir_stmt stmt)
       )
       (fun {edge_data; _} ->
          edge_data
          |> Poly_set.to_list
          |> String.concat ~sep:", "
          |> (fun s -> "{" ^ s ^ "}")
       )
    );
  Out_channel.close cout

(* Do live variable analysis on a list of IR statements *)
let analyze_live_vars (stmts : ir_stmt list) =
  let (cfg, node_lst, _, _) =
    Dataflow.generate_cfg
      ~values:stmts
      ~add_succs_func:Dataflow.add_ir_succs
      ~top_func
  in
  let data_store = Dataflow.analyze cfg
      ~eq:Poly_set.equal
      ~dir:`Backward
      ~transfer:transfer
      ~meet:meet
      ~top_func:top_func
  in
  (cfg, node_lst, data_store)

let analyze_dead_code (module Config : ConfigSpec) (`IRFunc (f, `IRSeq stmts, n_args, n_rets)) =
  let dead_code_exists = ref true in
  let updated_stmts = ref stmts in
  (* As long as some dead code was found, analyze the IR statements and remove
     dead code, because removing dead code could cause more dead code that
     can only be caught by re-running the analysis to get a new set of live vars *)
  while !dead_code_exists do
    dead_code_exists := false;
    let (cfg, node_lst, data_store) = analyze_live_vars !updated_stmts in
    (* Mark which nodes are dead and update the [updated_stmts] ... *)
    List.map node_lst
      ~f:(fun ({node_id; node_data} as node) ->
          (* Keep node alive if it has _ARG or _RET in it *)
          let defs = defs_of_stmt node_data in
          let uses = uses_of_stmt node_data in
          if Poly_set.exists (Poly_set.union defs uses) ~f:(fun str ->
              String.is_prefix str ~prefix:"(IRTemp _ARG" ||
              String.is_prefix str ~prefix:"(IRTemp _RET")
          then
            (`Alive, node)
          else (
            (* Check if a node [n] is "dead", i.e. [n] defines something which is
                not live going out of [n].
               NOTE: There should be *at most* 1 def per node *)
            let node_is_dead =
              match Poly_set.to_list defs with
              | [] -> false
              | [temp] ->
                (* NOTE: The data_store is a set of "live_in"s *)
                let live_out = List.fold (Poly_graph.succ_nodes cfg node_id)
                    ~init:(top_func ())
                    ~f:(fun acc succ_node_id ->
                        meet acc (Hash_table.find_exn data_store succ_node_id)
                      )
                in
                let is_dead = not (Poly_set.mem live_out temp) in
                dead_code_exists := !dead_code_exists || is_dead;
                is_dead
              | _  -> failwith (Printf.sprintf
                                  "Found multiple defs in one stmt: %s"
                                  (string_of_cfg_data node_data string_of_ir_stmt))
            in
            ((if node_is_dead then `Dead else `Alive), node)
          )


        )
    (* ... then filter out the IR statements which are alive ... *)
    |> List.filter_map ~f:(fun (status, {node_data; _}) ->
        match (status, node_data) with
        | (`Dead, _)
        | (`Alive, `Start)
        | (`Alive, `Exit)  -> None
        | (`Alive, `Other stmt) -> Some stmt
      )
    (* ... then update [updated_stmts]. *)
    |> (:=) updated_stmts;
  done;


  (* If specified, print the CFG after optimization *)
  if Hash_table.find_exn Config.optcfg_flags "dce" then (
    let (cfg, _, _, _) =
      Dataflow.generate_cfg
        ~values:!updated_stmts
        ~add_succs_func:Dataflow.add_ir_succs
        ~top_func
    in
    display cfg (Config.diag_out_path ^ "_" ^ f ^ "_dce.dot")
  );

  (* Return the function declaration with the updated statements *)
  `IRFunc (f, `IRSeq !updated_stmts, n_args, n_rets)

let main (module Config : ConfigSpec) ir =
  (* If specified, don't perform the optimization *)
  if not (Hash_table.find_exn Config.enabled_opts "dce") then
    ir
  else (
    debug 2 "eliminating dead code...";
    let `IRCompUnit (id, funcs, ctors, glob_inits) = ir in
    `IRCompUnit (id, List.map funcs ~f:(analyze_dead_code (module Config)), ctors, glob_inits)
  )

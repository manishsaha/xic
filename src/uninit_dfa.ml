open Common
open Core
open Graph_lib
open Ir

let ir_locations = ir_empty_loc_table ()

let meet = Poly_set.inter

(* Remove all IRTemps with _ARG or _RET prefix from set *)
let remove_args_rets set =
  let args_rets = empty () in
  Poly_set.iter set ~f:(fun var ->
      if String.is_prefix var ~prefix:"(IRTemp _ARG" ||
         String.is_prefix var ~prefix:"(IRTemp _RET" then
        Poly_set.add args_rets var
    );
  Poly_set.iter args_rets ~f:(Poly_set.remove set)

(* Transfer function simply removes the defs from the set of definitely
   uninitialized variables, because we know now these defs are possibly
   initialized *)
let transfer {node_data; _} l =
  let defs = defs_of_stmt node_data in
  Poly_set.diff l defs

(* Run the "Definitely Uninitialized" variable analysis. The goal is to
   find all the uninitialized variables.
   T = {all vars}
   Ordering = Subset
   Meet = Intersection
   F_n(l) = l - defs(n)
*)
let analyze_func (`IRFunc (_, `IRSeq stmts, _, _)) =
  (* Get all variables used/defined in the function *)
  let all_vars = empty () in
  List.iter stmts ~f:(fun stmt ->
      let defs = defs_of_stmt (`Other stmt) in
      let uses = uses_of_stmt (`Other stmt) in

      Poly_set.union_inplace all_vars defs;
      Poly_set.union_inplace all_vars uses
    );
  remove_args_rets all_vars;
  let top_func () = Poly_set.copy all_vars in
  let (cfg, node_lst, _, _) =
    Dataflow.generate_cfg
      ~values:stmts
      ~add_succs_func:Dataflow.add_ir_succs
      ~top_func
  in
  let data_store = Dataflow.analyze cfg
      ~eq:Poly_set.equal
      ~dir:`Forward
      ~transfer:transfer
      ~meet:meet
      ~top_func:top_func
  in

  (* Go through all the nodes, and if a node uses a variable that is
     definitely uninitialized, a warning should be displayed. *)
  List.iter node_lst ~f:(fun {node_id; node_data} ->
      let uses = uses_of_stmt node_data in
      let uninit_vars = Hash_table.find_exn data_store node_id in
      let uninit_used = Poly_set.inter uses uninit_vars in
      if not (Poly_set.equal (empty ()) uninit_used) then (
        (* Helper function to find the temps in statements/exprs that are uninitialized *)
        let rec temps_of_stmt s : ir_expr list = 
          match s with
          | `IRMove (e1, e2) -> List.concat [temps_of_expr e1; temps_of_expr e2]
          | `IRExp e1
          | `IRJump e1
          | `IRCJump (e1,_,_) -> temps_of_expr e1
          | `IRSeq ss -> List.concat_map ss ~f:temps_of_stmt
          | `IRReturn es -> List.concat_map es ~f:temps_of_expr
          | `IRLabel _ -> []
        and temps_of_expr (e:ir_expr) : ir_expr list = 
          match e with
          | `IRTemp _ when Poly_set.mem uninit_used (string_of_ir_expr e) -> [e]
          | `IRBinary (_,e1,e2) -> List.concat [temps_of_expr e1; temps_of_expr e2]
          | `IRMem e1 -> temps_of_expr e1
          | `IRCall (e1,es,_,_) -> List.concat [temps_of_expr e1; List.concat_map es ~f:temps_of_expr]
          | _ -> []
        in
        let s = match node_data with
          | `Other s' -> s'
          | _ -> failwith surprised_pikachu
        in
        let all = temps_of_stmt s in
        (* Printf.printf "Warning: Statement uses uninitialized variable(s):%s" *)
          (* (string_of_cfg_data node_data string_of_ir_stmt) *)
          (List.iter all ~f:(fun e ->
               let (f,r,c) = location_of_ir_node ir_locations e in
               if r >= 0 && c >= 0
               then (
                 Printf.printf "%s\tLn %i, Col %i\tWarning: variable '%s' used before initialization\n"
                   f r c (string_of_ir_node e
                   |> String.chop_prefix_exn ~prefix:"(IRTemp $"
                   |> String.chop_suffix_exn ~suffix:")"))
               else (
                 Printf.printf "%s\tLn ?, Col ?\tWarning: variable '%s' used before initialization\n"
                   f (string_of_ir_node e
                   |> String.chop_prefix_exn ~prefix:"(IRTemp $"
                   |> String.chop_suffix_exn ~suffix:")"))
          )
          )

          (* (location_of_ir_node ir_locations ) *)
      )
      else ()
    )

(* Main function to run the uninitialized variable analysis *)
let main (module Config : ConfigSpec) locs (`IRCompUnit (id, funcs, ctors, glob_inits)) : ir_comp_unit =
  ir_copy_loc_table ~t_from:locs ~t_to:ir_locations;
  (* If "-noextension" is not specified, then run the analysis *)
  if not Config.noextension then (
    debug 2 "checking for uninitialized variables...";
    List.iter funcs ~f:analyze_func
  );
  `IRCompUnit (id, funcs, ctors, glob_inits)

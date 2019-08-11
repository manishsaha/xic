open Asm
open Cfg
open Common
open Core
open Graph_lib

let top_func () = Poly_set.create (module String)

let print_coloring coloring =
  Hash_table.iteri coloring
    ~f:(fun ~key ~data -> debug 3 "(%s, %s)" key (string_of_reg concrete_regs.(data)))

let print_reg_map reg_map =
  Hash_table.iteri reg_map ~f:(fun ~key ~data -> debug 3 "(%s, %s)" key data)

let display_inter_graph inter_graph file =
  let cout = Out_channel.create file in
  UDot.output_graph cout
    (ocamlgraph_of_graph inter_graph (const "") (const ""));
  Out_channel.close cout

(* Generate the CFG from the asm list. *)
let init_cfg asms =
  let (cfg, _, _, _) =
    Dataflow.generate_cfg
      ~add_succs_func:Dataflow.add_asm_succs
      ~top_func
      ~values:(List.filter asms ~f:(fun asm ->
          match asm with
          | `Comment _ -> false
          | _ -> true
        ))
  in
  cfg

(* Run the live variable analysis on the cfg. *)
let analyze_live_vars cfg =
  let data_store =
    Dataflow.analyze cfg
      ~eq:Poly_set.equal
      ~dir:`Backward
      ~transfer:(fun {node_data; _} l ->
          (* F_n(l) = use(n) U (l - def(n)) *)
          let (written, read) =
            match node_data with
            | `Other (#instr as ins) -> get_regs_used ins
            | `Start -> (Array.to_list callee_saved, [])
            | _ -> ([], [])
          in
          (* Turn registers into a set by making them strings *)
          let defs = set_of_reg_list written in
          let uses = set_of_reg_list read in
          Poly_set.union uses (Poly_set.diff l defs)
        )
      ~meet:Poly_set.union
      ~top_func
  in
  data_store

(* Create the interference graph from the cfg. *)
let create_interference_graph all_regs cfg data_store =
  (* Add all (distinct) registers used in all instructions as nodes in a graph. *)
  let inter_graph = Poly_graph.create ~directed:false in
  let reg_map = Hash_table.create (module String) in
  Poly_set.iter all_regs ~f:(fun reg ->
      Poly_graph.add_node_with_id inter_graph reg () |> ignore;
      Hash_table.set reg_map ~key:reg ~data:reg
    );
  (* Precolor all concrete register references + first 6 args + first 2 rets *)
  let coloring = Hash_table.create (module String) in
  Array.iteri concrete_regs
    ~f:(fun i reg -> Hash_table.set coloring ~key:(string_of_reg reg) ~data:i);
  (* Add edges in the interference graph based on which variables are live simultaneously *)
  Hash_table.iteri data_store ~f:(fun ~key:n_id ~data:live ->
      let {node_data = wrapped_asm; _} = Poly_graph.get_node cfg n_id in
      (* all defined concrete regs interfere with live-out regs *)
      begin
        match wrapped_asm with
        | `Other (#instr as instr) ->
          let (written, _) = get_regs_used instr in
          List.iter written ~f:(fun def_reg ->
              let def_reg_str = string_of_reg def_reg in
              if (
                Hash_table.mem coloring def_reg_str
                || String.is_prefix def_reg_str ~prefix:"_"
              ) then
                Poly_set.iter live ~f:(fun live_reg_str ->
                    Poly_graph.add_edge inter_graph () (live_reg_str, def_reg_str)
                    |> ignore
                  )
            );
        | _ -> ()
      end;
      live
      |> Poly_set.all_pairs
      |> List.iter ~f:(fun (n1_id, n2_id) ->
          try Poly_graph.add_edge inter_graph () (n1_id, n2_id) |> ignore
          with _ -> ()
        )
    );
  (inter_graph, reg_map, coloring)

let has_uncolored inter_graph coloring =
  Poly_graph.exists_node inter_graph ~f:(fun {node_id; _} ->
      not (Hash_table.mem coloring node_id)
    )

let build _ asms =
  debug 3 "building graphs from %d instruction%s..."
    (List.length asms) (if List.length asms = 1 then "" else "s");
  let cfg = init_cfg asms in
  let data_store = analyze_live_vars cfg in
  let all_regs = find_all_regs asms in
  let (inter_graph, reg_map, coloring) = create_interference_graph all_regs cfg data_store in
  (* the move graph has the same nodes as the interference graph, with edges from move nodes *)
  let move_graph = Poly_graph.create ~directed:false in
  Poly_graph.iter_nodes inter_graph ~f:(fun {node_id; node_data} ->
      Poly_graph.add_node_with_id move_graph node_id node_data |> ignore
    );
  List.iter asms ~f:(fun asm ->
      match asm with
      | `MOVQ ((#reg as r1), (#reg as r2)) ->
        Poly_graph.add_edge move_graph () (string_of_reg r1, string_of_reg r2) |> ignore
      | _ -> ()
    );
  debug 3 "done building";
  (inter_graph, move_graph, reg_map, coloring)

(* see Iterated Register Coalescing (Appel) for an explanation of the algorithm *)
(* simplify: remove a low-degree, non-move-related, non-colored node *)
let simplify inter_graph move_graph coloring k =
  let n_opt = Poly_graph.(
      find_node inter_graph ~f:(fun {node_id = n_id; _} ->
          degree inter_graph n_id <= k - 1
          && degree move_graph n_id = 0
          && not (Hash_table.mem coloring n_id)
        )
    )
  in
  Option.map n_opt ~f:(fun {node_id; _} ->
      debug 3 "simplifying node %s" node_id;
      Poly_graph.(
        remove_node move_graph node_id |> ignore;
        remove_node inter_graph node_id
      )
    )

(* coalesce: conservatively combine two move-related, non-interfering nodes *)
let coalesce inter_graph move_graph reg_map coloring k asms =
  let changed = ref false in
  let check_conservative n1_id n2_id =
    let high_deg_succs = Poly_set.create (module String) in
    List.iter
      (Poly_graph.succ_nodes inter_graph n1_id @ Poly_graph.succ_nodes inter_graph n2_id)
      ~f:(fun succ_id ->
          if not (Poly_set.mem high_deg_succs succ_id) then
            let deg =
              Poly_graph.(
                (* subtract 1 if both original nodes were neighbors (deg after merge) *)
                if (has_edge_with_endpoints inter_graph (n1_id, succ_id)
                    && has_edge_with_endpoints inter_graph (n2_id, succ_id)) then
                  degree inter_graph succ_id - 1
                else
                  degree inter_graph succ_id
              )
            in
            if deg >= k then Poly_set.add high_deg_succs succ_id
        );
    Poly_set.length high_deg_succs < k
  in
  let nodes = 
    List.filter_map asms ~f:(fun asm ->
        match asm with
        | `MOVQ ((#reg as r1), (#reg as r2)) ->
          let r1_str = string_of_reg r1 in
          let r2_str = string_of_reg r2 in
          let n1_id = Hash_table.find_exn reg_map r1_str in
          let n2_id = Hash_table.find_exn reg_map r2_str in
          let can_coalesce =
            Poly_graph.(
              n1_id <> n2_id
              && has_uncolored inter_graph coloring
              && has_node inter_graph n1_id
              && has_node inter_graph n2_id
              && not (has_edge_with_endpoints inter_graph (n1_id, n2_id))
              && check_conservative n1_id n2_id
            )
          in
          let c1_opt = Hash_table.find coloring n1_id in
          let c2_opt = Hash_table.find coloring n2_id in
          let can_coalesce = 
            match (c1_opt, c2_opt) with
            | (Some c1, Some c2) when c1 <> c2 -> false
            | _ -> can_coalesce
          in
          if can_coalesce then (
            debug 3 "coalescing %s and %s" n1_id n2_id;
            if n1_id = n2_id then () else (
              (* merge nodes in interference + move graphs *)
              let n_id = Poly_graph.merge inter_graph n1_id n2_id ~combine:(fun _ _ -> ()) in
              begin
                match (c1_opt, c2_opt) with
                | (Some c, _)
                | (_, Some c) -> Hash_table.set coloring ~key:n_id ~data:c
                | _ -> ()
              end;
              Poly_graph.merge move_graph n1_id n2_id ~combine:(fun _ _ -> ()) |> ignore;
              (* update reg_map to track new node also *)
              Hash_table.map_inplace reg_map ~f:(fun old_n_id ->
                  if old_n_id = n1_id || old_n_id = n2_id then n_id else old_n_id
                );
              Hash_table.set reg_map ~key:n_id ~data:n_id;
              changed := true
            );
            None
          ) else Some asm
        | _ -> Some asm
      )
  in
  (nodes, !changed)

(* freeze: unmark nodes as move-related (give up hope of coalescing them) *)
let freeze inter_graph move_graph coloring k =
  let node_opt =
    Poly_graph.find_node move_graph ~f:(fun {node_id; _} ->
        not (Hash_table.mem coloring node_id)
        && Poly_graph.degree move_graph node_id > 0
        && Poly_graph.degree inter_graph node_id < k
      )
  in
  begin
    match node_opt with
    | Some {node_id; _} ->
      debug 3 "freezing %s" node_id;
      List.iter
        (Poly_graph.succ_edges move_graph node_id)
        ~f:(compose ignore (Poly_graph.remove_edge move_graph));
      true
    | _ -> false
  end

(* select: try find a color for a simplified node *)
let select move_graph reg_map coloring k (node, _, succs) =
  let n_id = node.node_id in
  let taken =
    succs
    |> List.filter_map ~f:(fun succ ->
        succ
        |> Hash_table.find_exn reg_map
        |> Hash_table.find coloring
      )
    |> Poly_set.of_list (module Int)
  in
  let best =
    n_id
    |> Poly_graph.succ_nodes move_graph
    |> List.filter_map ~f:(fun succ ->
        succ
        |> Hash_table.find_exn reg_map
        |> Hash_table.find coloring
      )
    |> Poly_set.of_list (module Int)
    |> (fun best -> Poly_set.diff best taken)
  in
  let color_opt =
    if Poly_set.is_empty best then
      k
      |> List.init ~f:ident
      |> List.find ~f:(non (Poly_set.mem taken))
    else
      Poly_set.find best ~f:(const true)
  in
  Option.map color_opt ~f:(fun color ->
      debug 3 "coloring %s with %d = %s"
        n_id color (string_of_reg concrete_regs.(color));
      Hash_table.set coloring ~key:n_id ~data:color;
      color
    )

(* spill: give a stack location to a node that can't be colored by the above and rewrite
 * asms to load/store as needed *)
let spill reg_map spilled_map asms =
  List.(
    concat_map asms ~f:(fun asm ->
        match asm with
        | #instr as instr ->
          let (written, read) = get_regs_used instr in
          let spilled regs =
            filter_map regs ~f:(fun reg ->
                let reg_str = string_of_reg reg in
                let coalesced = Hash_table.find_exn reg_map reg_str in
                if Hash_table.mem spilled_map coalesced then Some reg_str else None
              )
          in
          let spilled_written = spilled written in
          let spilled_read = spilled read in
          let replace_map = Hash_table.create (module String) in
          let loc_map = Hash_table.create (module String) in
          iteri (spilled_written @ spilled_read) ~f:(fun i r_str ->
              if not (Hash_table.mem replace_map r_str) then (
                Hash_table.set replace_map ~key:r_str ~data:spill_regs.(i);
                let index =
                  r_str
                  |> Hash_table.find_exn reg_map
                  |> Hash_table.find_exn spilled_map
                in
                let loc = make_addr ~base:`RBP ~displacement:(to_offset ~-(index + 1)) () in
                Hash_table.set loc_map ~key:r_str ~data:loc
              )
            );
          concat [
            map spilled_read ~f:(fun r_str ->
                `MOVQ (
                  (Hash_table.find_exn replace_map r_str :> loc),
                  (Hash_table.find_exn loc_map r_str :> arg)
                )
              );
            [(replace_instr replace_map instr :> asm)];
            map spilled_written ~f:(fun r_str ->
                `MOVQ (
                  (Hash_table.find_exn loc_map r_str :> loc),
                  (Hash_table.find_exn replace_map r_str :> arg)
                )
              )
          ]
        | _ -> [asm]
      )
  )

let main f asms =
  let spilled_map = Hash_table.create (module String) in
  let max_stack_loc = ref 0 in
  let k = Array.length var_regs in
  let find_loc succs =
    let taken_locs = List.filter_map succs ~f:(Hash_table.find spilled_map) in
    let loc_opt =
      List.(
        init (!max_stack_loc + 1) ~f:ident
        |> find ~f:(non (mem taken_locs ~equal:(=)))
      )
    in
    match loc_opt with
    | None -> incr max_stack_loc; !max_stack_loc
    | Some loc -> loc
  in

  (* run the algorithm -- originally it ran multiple times (until completion) for the best
   * output but this made compilation take too long (often not terminating) due to the time taken
   * to generate the CFG, run the live variable analysis, and build the interference graph *)
  let start asms =
    let (inter_graph, move_graph, reg_map, coloring) = build f asms in
    let stack = Stack.create () in

    let rec iter_simplify asms =
      match simplify inter_graph move_graph coloring k with
      | Some removed -> Stack.push stack removed; iter_simplify asms 
      | None -> iter_coalesce asms

    and iter_coalesce asms =
      let (new_asms, changed) = coalesce inter_graph move_graph reg_map coloring k asms in
      if changed then iter_simplify new_asms else iter_freeze asms

    and iter_freeze asms =
      if freeze inter_graph move_graph coloring k then (
        iter_simplify asms
      ) else if has_uncolored inter_graph coloring then (
        let {node_id; _} =
          Option.value_exn
            (Poly_graph.find_node inter_graph ~f:(fun {node_id; _} ->
                 not (Hash_table.mem coloring node_id)
               )
            )
        in
        Stack.push stack (Poly_graph.remove_node inter_graph node_id);
        iter_simplify asms
      ) else (
        iter_select asms
      )

    and iter_select asms =
      while not (Stack.is_empty stack) do
        stack
        |> Stack.pop
        |> Option.iter ~f:(fun removed ->
            match select move_graph reg_map coloring k removed with
            | None ->
              let ({node_id = n_id; _}, _, succs) = removed in
              let loc = find_loc succs in
              debug 3 "spilling node %s to %d" n_id loc;
              Hash_table.set spilled_map ~key:n_id ~data:loc
            | Some _ -> ()
          )
      done;
      asms
      |> spill reg_map spilled_map
      |> iter_alloc 

    and iter_alloc asms =
      debug 3 "allocating %d instructions" (List.length asms);
      let alloc_map =
        Hash_table.filter_map reg_map ~f:(fun n_id ->
            n_id
            |> Hash_table.find coloring 
            |> Option.map ~f:(Array.get concrete_regs)
          )
      in
      let l = if Hash_table.is_empty spilled_map then 0 else !max_stack_loc + 1 in
      List.(
        asms
        |> cons (`ENTER (`Const (to_offset (l + l % 2)), `Const 0L))
        |> concat_map ~f:(fun asm ->
              match asm with
              | `RET -> [`LEAVE; `RET]
              | _ -> [asm]
          )
        |> map ~f:(fun asm ->
            match asm with
            | #instr as instr -> (replace_instr alloc_map instr :> asm)
            | _ -> asm
          )
        |> filter ~f:(fun asm ->
            match asm with
            | `MOVQ (#loc as arg1, arg2) when arg1 = arg2 -> false
            | _ -> true
          )
      )
    in
    iter_simplify asms
  in
  start asms

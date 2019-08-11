open Asm
open Common
open Core
open Graph_lib

type 'a cfg_data = [ `Start | `Exit | `Other of 'a ] [@@deriving variants]

(** Generate a CFG given:
    [values] - a list of some values
    [add_succs] - a function that adds all the successors of a graph node,
      based on *just* the data in the node (not based on any edges in the graph,
      because no edges have been created yet)
    [top_func] - a thunk to generate the "top" element of the lattice implicitly defined by ['e]
*)
let generate_cfg ~values ~add_succs_func ~top_func =
  (* Create an empty graph *)
  let graph = Poly_graph.create ~directed:true in
  (* Add all the values to the graph and save the ids given to each value *)
  let node_lst =
    List.(
      values
      |> map ~f:(fun value ->
          let node_data = `Other value in
          let node_id = Poly_graph.add_node graph node_data in
          {node_id; node_data}
        );
    )
  in
  (* Add the START and EXIT nodes to the graph *)
  let start_id = Poly_graph.add_node graph `Start in
  let exit_id = Poly_graph.add_node graph `Exit in
  let nodes = List.concat [
      [{node_id = start_id; node_data = `Start}];
      node_lst;
      [{node_id = exit_id; node_data = `Exit}]
    ] in
  (* Add the edges to the graph, with [top] as the default value on the edge *)
  add_succs_func graph nodes start_id exit_id top_func;
  (graph, node_lst, start_id, exit_id)

(** Implementation of the worklist algorithm (without considering SCCs).
    NOTE: All the out edges of a single node have the same value
*)
let analyze graph ~eq ~dir ~transfer ~meet ~top_func =
  (* Create a worklist as a LinkedHashSet, i.e. a map from (the index of) each
     node [n] to unit and initialize it to contain all nodes in cfg. *)
  let module Set_queue = Hash_queue.Make (String) in
  (* initialize worklist in post-order/reverse-post-order *)
  let start_id =
    (Option.value_exn
       (Poly_graph.find_node graph ~f:(fun {node_data; _} -> node_data = `Start))
    ).node_id
  in
  let node_order =
    Poly_graph.fold graph
      ~order:`Post
      ~init:[]
      ~start:start_id
      ~f_node:(fun acc {node_id; _} -> node_id :: acc)
      ~f_edge:(fun acc _ -> acc)
    |> (if dir = `Forward then ident else List.rev)
  in
  let worklist = Set_queue.create () in
  let data_store = Hash_table.create (module String) in
  List.iter node_order ~f:(fun n_id ->
      Set_queue.enqueue_exn worklist n_id n_id;
      (* Hash_table.set data_store ~key:n_id ~data:(top_func ()) *)
    );
  Poly_graph.iter_edges graph ~f:(fun {edge_data; endpoints = (n1_id, n2_id); _} ->
      let n_id = if dir = `Forward then n1_id else n2_id in
      Hash_table.set data_store ~key:n_id ~data:edge_data
    );
  (* Continue to update out(n) for some node [n] until there is nothing
     in the worklist *)
  while not (Set_queue.is_empty worklist) do
    (* Remove an id from the worklist *)
    let node_id = Set_queue.dequeue_exn worklist in
    let node = Poly_graph.get_node graph node_id in
    let old_data = Hash_table.find_or_add data_store node_id ~default:top_func in
    (* For forward analysis, compute:
       in[n] = F(meet over all predecessors n' of n of out(n'))
       For backward analysis, compute:
       out[n] = F(meet over all successors n' of n of out(n')) *)
    let prev_ids =
      if dir = `Forward
      then Poly_graph.pred_nodes graph node_id
      else Poly_graph.succ_nodes graph node_id
    in
    let next_ids =
      if dir = `Forward
      then Poly_graph.succ_nodes graph node_id
      else Poly_graph.pred_nodes graph node_id
    in
    let input =
      List.fold prev_ids ~init:(top_func ()) ~f:(fun acc id ->
          meet acc (Hash_table.find_exn data_store id)
        )
    in
    let new_data = transfer node input in
    if not (eq new_data old_data) then (
      Hash_table.set data_store ~key:node_id ~data:new_data;
      List.iter next_ids ~f:(fun id -> Set_queue.enqueue worklist id id |> ignore)
    )
  done;
  Poly_graph.iter_edges graph ~f:(fun {edge_id; endpoints = (n1_id, n2_id); _} ->
      let data = Hash_table.find_exn data_store (if dir = `Forward then n1_id else n2_id) in
      Poly_graph.update_edge graph edge_id ~new_data:data
    );
  data_store

(* Helper function to add nodes to a graph using successor function *)
let add_succs graph nodes start_id exit_id other_succs top_func =
  let node_id_arr =
    nodes
    |> List.map ~f:(fun {node_id; _} -> node_id)
    |> Array.of_list
  in
  List.iteri nodes ~f:(fun idx {node_id; node_data} ->
      let succs =
        match node_data with
        | `Start -> [node_id_arr.(idx + 1)] (* Next node *)
        | `Exit -> [] (* No successors *)
        | `Other other -> other_succs node_id_arr start_id exit_id idx other
      in
      List.iter succs ~f:(fun succ_id ->
          Poly_graph.add_edge graph (top_func ()) (node_id, succ_id) |> ignore
        )
    )

(* Add the successors of each ASM node in [nodes] into [graph] *)
let add_asm_succs graph nodes start_id exit_id top_func =
  let asm_succs =
    let label_to_id_map = Hash_table.create (module String) in
    List.iter nodes ~f:(fun {node_id; node_data} ->
        match node_data with
        | `Other (`Label l) -> Hash_table.set label_to_id_map ~key:l ~data:node_id
        | _ -> ()
      );
    fun node_id_arr _ exit_id idx (asm : asm) ->
      match asm with
      | `RET -> [exit_id] (* Exit node *)
      | `JMP (`Label l) -> [Hash_table.find_exn label_to_id_map l] (* Label node *)
      | `JE (`Label l)
      | `JNE (`Label l)
      | `JG (`Label l)
      | `JGE (`Label l)
      | `JL (`Label l)
      | `JLE (`Label l) -> [node_id_arr.(idx + 1); Hash_table.find_exn label_to_id_map l]
      | _ -> [node_id_arr.(idx + 1)] (* Next node *)
  in
  add_succs graph nodes start_id exit_id asm_succs top_func

(* Add the successors of each IR node in [nodes] into [graph] *)
let add_ir_succs graph nodes start_id exit_id top_func =
  let ir_succs =
    let label_to_id_map = Hash_table.create (module String) in
    List.iter nodes ~f:(fun {node_id; node_data} ->
        match node_data with
        | `Other (`IRLabel l) -> Hash_table.set label_to_id_map ~key:l ~data:node_id
        | _ -> ()
      );
    fun node_id_arr _ exit_id idx stmt ->
      match stmt with
      | `IRJump (`IRName l) -> [Hash_table.find_exn label_to_id_map l]
      | `IRReturn _ -> [exit_id]
      (* Next node and the corresponding label node. Ignore the 2nd label,
               since the the next instruction in the LIR should be a jump to it *)
      | `IRCJump (_, l1, _) -> [node_id_arr.(idx + 1); Hash_table.find_exn label_to_id_map l1]
      | _ -> [node_id_arr.(idx + 1)]
  in
  add_succs graph nodes start_id exit_id ir_succs top_func

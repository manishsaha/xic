open Common 
open Core
open Graph_lib

(* Module representation of a Node *)
module Node = struct
  type t = id * string
  let compare (id1, _) (id2, _) = Pervasives.compare id1 id2
  let hash (id, _) = Hash_table.hash id
  let equal (id1, _) (id2, _) = (id1 = id2)
end

(* representation of an edge -- must be comparable *)
module Edge = struct
  type t = string
  let compare = Pervasives.compare
  let equal = (=)
  let default = ""
end

module StringGraph = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

(* Take our representation of a graph [g] and turn it into an ocamlgraph *)
let ocamlgraph_of_graph graph string_of_node string_of_edge =
  let sg = ref StringGraph.empty in
  Poly_graph.iter_nodes graph ~f:(fun node ->
      sg := StringGraph.add_vertex !sg (node.node_id, string_of_node node)
    );
  Poly_graph.iter_edges graph ~f:(fun edge ->
      let (id_from, id_to) = edge.endpoints in
      let v_from = (id_from, Poly_graph.get_node graph id_from |> string_of_node) in
      let v_to = (id_to, Poly_graph.get_node graph id_to |> string_of_node) in
      sg := StringGraph.add_edge_e !sg (v_from, string_of_edge edge, v_to)
    );
  !sg

(* Modified from:
   https://stackoverflow.com/questions/8999557/how-to-visualize-draw-automata-in-ocaml *)

module type Directed = sig
  val directed : bool
end

(* Module for creating Graphviz dot files for a directed/undirected graph *)
module DotSpec (D : Directed) = struct
  include Graph.Graphviz.Dot(
    struct
      include StringGraph
      let edge_attributes (_, e, _) = [
        `Label e;
        `Color 4711;
        if D.directed
        then `Arrowhead `Normal
        else `Arrowhead `None;
      ]
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_name (id, _) = "\"" ^ id ^ "\""
      let vertex_attributes (id, l) = [`Label (Printf.sprintf "(%s) %s" id l); `Shape `Box]
      let default_vertex_attributes _ = []
      let graph_attributes _ = []
    end)
  let output_graph cout graph = output_graph cout graph; Printf.fprintf cout "\n"
end

module DDot = DotSpec(struct let directed = true end)

module UDot = DotSpec(struct let directed = false end)

(* Convert a CFG node into a string *)
let string_of_cfg_data data str_fn =
  match data with
  | `Start -> "START"
  | `Exit  -> "EXIT"
  | `Other other -> str_fn other

(* Output a dot file for an IR CFG to [file] *)
let display_ir_set_graph graph file =
  let cout = Out_channel.create file in
  DDot.output_graph cout
    (ocamlgraph_of_graph graph
       (fun {node_data; _} ->
          string_of_cfg_data node_data (compose String.strip Ir.string_of_ir_stmt))
       (fun {edge_data; _} ->
          edge_data
          |> Poly_set.to_list
          |> String.concat ~sep:",\n"
          |> (fun s -> "{" ^ s ^ "}")));
  Out_channel.close cout

(* Output a dot file for an ASM CFG to [file] *)
let display_cfg cfg file =
  let cout = Out_channel.create file in
  DDot.output_graph cout
    (ocamlgraph_of_graph cfg
       (fun {node_data; _} ->
          string_of_cfg_data node_data (compose String.strip Asm.string_of_asm))
       (fun {edge_data; _} ->
          edge_data
          |> Poly_set.to_list
          |> String.concat ~sep:", "
          |> (fun s -> "{" ^ s ^ "}")));
  Out_channel.close cout

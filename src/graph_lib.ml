open Common
open Core

type 'n node = {
  node_id : id;
  node_data : 'n;
}

type 'e edge = {
  edge_id : id;
  endpoints : id * id;
  edge_data : 'e;
}

type ('n, 'e) graph = {
  directed : bool;
  mutable nodes : (string, 'n node) Hash_table.t;
  mutable edges : (string, 'e edge) Hash_table.t;
  mutable next_node_id : int;
  mutable adj_list : (string, string Poly_set.t) Hash_table.t; (* node id to list of edge ids *)
}

module Poly_graph = struct
  let create ~directed = {
    directed;
    nodes = Hash_table.create (module String);
    edges = Hash_table.create (module String);
    next_node_id = 0;
    adj_list = Hash_table.create (module String);
  }

  let edge_id_of_endpoints graph (n1_id, n2_id) =
    let reorder = not graph.directed && String.compare n1_id n2_id > 0 in
    Printf.sprintf "%s -> %s" (if reorder then n2_id else n1_id) (if reorder then n1_id else n2_id)

  let n_nodes {nodes; _} = Hash_table.length nodes
  let n_edges {edges; _} = Hash_table.length edges

  let get_node {nodes; _} node_id = Hash_table.find_exn nodes node_id
  let get_edge {edges; _} edge_id = Hash_table.find_exn edges edge_id
  let get_edge_with_endpoints graph endpoints =
    get_edge graph (edge_id_of_endpoints graph endpoints)

  let has_node {nodes; _} node_id = Hash_table.mem nodes node_id
  let has_edge {edges; _} edge_id = Hash_table.mem edges edge_id
  let has_edge_with_endpoints graph endpoints =
    has_edge graph (edge_id_of_endpoints graph endpoints)

  let next_node_id graph =
    let node_id = graph.next_node_id in
    graph.next_node_id <- graph.next_node_id + 1;
    Int.to_string node_id

  let add_node_with_id graph node_id node_data =
    if has_node graph node_id then false
    else (
      let {adj_list; nodes; _} = graph in
      let node = {node_id; node_data} in
      Hash_table.set adj_list ~key:node_id ~data:(Poly_set.create (module String));
      Hash_table.set nodes ~key:node_id ~data:node;
      true
    )

  let add_node graph node_data =
    let node_id = next_node_id graph in
    add_node_with_id graph node_id node_data |> ignore;
    node_id

  let add_edge graph edge_data endpoints =
    let (n1_id, n2_id) = endpoints in
    let edge_id = edge_id_of_endpoints graph endpoints in
    if n1_id = n2_id || has_edge graph edge_id then false
    else (
      let {adj_list; edges; _} = graph in
      let n1_edge_ids = Hash_table.find_exn adj_list n1_id in
      let n2_edge_ids = Hash_table.find_exn adj_list n2_id in
      let edge = {edge_id; edge_data; endpoints} in
      Poly_set.add n1_edge_ids edge_id;
      Poly_set.add n2_edge_ids edge_id;
      Hash_table.set edges ~key:edge_id ~data:edge;
      true  
    ) 

  let random_node_id {nodes; _} =
    With_return.with_return
      (fun r -> Hash_table.iter_keys nodes ~f:(fun n_id -> r.return n_id); failwith "empty")

  let node_edges {adj_list; _} node_id =
    node_id
    |> Hash_table.find_exn adj_list
    |> Poly_set.to_list

  let succ_edges graph node_id =
    node_id
    |> node_edges graph
    |> List.filter ~f:(fun edge_id ->
        let {endpoints = (n1_id, n2_id); _} = get_edge graph edge_id in
        n1_id = node_id || (not graph.directed && n2_id = node_id)
      )

  let succ_nodes graph node_id =
    node_id
    |> succ_edges graph
    |> List.map ~f:(fun edge_id ->
        let {endpoints = (n1_id, n2_id); _} = get_edge graph edge_id in
        if n1_id = node_id then n2_id else n1_id
      )

  let pred_edges graph node_id =
    node_id
    |> node_edges graph
    |> List.filter ~f:(fun edge_id ->
        let {endpoints = (n1_id, n2_id); _} = get_edge graph edge_id in
        n2_id = node_id || (not graph.directed && n1_id = node_id)
      )

  let pred_nodes graph node_id =
    node_id
    |> pred_edges graph
    |> List.map ~f:(fun edge_id ->
        let {endpoints = (n1_id, n2_id); _} = get_edge graph edge_id in
        if n1_id = node_id then n2_id else n1_id
      )

  let remove_edge graph edge_id =
    let {adj_list; edges; _} = graph in
    let edge = get_edge graph edge_id in
    let (n1_id, n2_id) = edge.endpoints in
    Hash_table.(
      Poly_set.remove (find_exn adj_list n1_id) edge_id;
      Poly_set.remove (find_exn adj_list n2_id) edge_id;
      remove edges edge_id;
    );
    edge

  let remove_node graph node_id =
    let {nodes; adj_list; _} = graph in
    let node = get_node graph node_id in
    let succs = succ_nodes graph node_id in
    let edges =
      node_id
      |> Hash_table.find_exn adj_list
      |> Poly_set.to_list
      |> List.map ~f:(remove_edge graph)
    in
    Hash_table.remove nodes node_id;
    (node, edges, succs)

  let restore_node graph ({node_id = n_id; node_data}, edges, _) =
    if add_node_with_id graph n_id node_data then
      List.map edges ~f:(fun {edge_data; endpoints = (n1_id, n2_id); _} ->
          let new_endpoints = if n_id = n1_id then (n_id, n2_id) else (n1_id, n_id) in
          add_edge graph edge_data new_endpoints
        )
      |> List.for_all ~f:ident
    else false

  let insert_before graph base_n_id n_data e_data =
    let new_n_id = add_node graph n_data in
    base_n_id
    |> pred_nodes graph
    |> List.iter ~f:(fun pred_n_id ->
        let {edge_data; _} =
          remove_edge graph (edge_id_of_endpoints graph (pred_n_id, base_n_id))
        in
        add_edge graph edge_data (pred_n_id, new_n_id) |> ignore
      );
    add_edge graph e_data (new_n_id, base_n_id) |> ignore;
    new_n_id
    
  let insert_after graph base_n_id n_data e_data =
    let new_n_id = add_node graph n_data in
    base_n_id
    |> succ_nodes graph
    |> List.iter ~f:(fun succ_n_id ->
        let {edge_data; _} =
          remove_edge graph (edge_id_of_endpoints graph (base_n_id, succ_n_id))
        in
        add_edge graph edge_data (new_n_id, succ_n_id) |> ignore
      );
    add_edge graph e_data (base_n_id, new_n_id) |> ignore;
    new_n_id

  let fold
      graph
      ~order:(order : [ `Pre | `Post ])
      ~init:(init : 'a)
      ~start:(start : id)
      ~f_node:(f_node : 'a -> 'n node -> 'a)
      ~f_edge:(f_edge : 'a -> 'e edge -> 'a)
    : 'a =
    let visited_node_ids = Poly_set.create (module String) in
    let visited_edge_ids = Poly_set.create (module String) in
    let rec dfs_node acc node_id =
      if Poly_set.mem visited_node_ids node_id then acc else (
        Poly_set.add visited_node_ids node_id;
        let node = get_node graph node_id in
        let node_edges = succ_edges graph node_id in
        match order with
        | `Pre -> List.fold node_edges ~init:(f_node acc node) ~f:(dfs_edge node_id)
        | `Post -> f_node (List.fold node_edges ~init:acc ~f:(dfs_edge node_id)) node
      )
    and dfs_edge node_id acc edge_id =
      if Poly_set.mem visited_edge_ids edge_id then acc else (
        Poly_set.add visited_edge_ids edge_id;
        let edge = get_edge graph edge_id in
        let {endpoints = (n1_id, n2_id); _} = edge in
        let next_node_id = if node_id = n1_id then n2_id else n1_id in
        match order with
        | `Pre -> dfs_node (f_edge acc edge) next_node_id
        | `Post -> f_edge (dfs_node acc next_node_id) edge
      )
    in
    dfs_node init start

  let copy {directed; nodes; edges; next_node_id; adj_list} =
    let adj_list = Hash_table.map adj_list ~f:Poly_set.copy in
    {
      directed;
      next_node_id;
      adj_list;
      nodes = Hash_table.copy nodes;
      edges = Hash_table.copy edges;
    }

  let map_inplace graph ~f_node ~f_edge =
    graph.nodes <- Hash_table.map graph.nodes ~f:f_node;
    graph.edges <- Hash_table.map graph.edges ~f:f_edge

  let map graph = graph |> copy |> map_inplace

  let merge graph n1_id n2_id ~combine =
    let n1 = get_node graph n1_id in
    let n2 = get_node graph n2_id in
    let n_id = n1_id ^ "|" ^ n2_id in
    add_node_with_id graph n_id (combine n1.node_data n2.node_data) |> ignore;
    let edge_id = edge_id_of_endpoints graph (n1_id, n2_id) in
    if has_edge graph edge_id then remove_edge graph edge_id |> ignore else ();
    let replace_id old_n_id new_n_id n_id =
      if n_id = old_n_id then new_n_id else n_id
    in
    let replace_edge old_n_id new_n_id edge_id =
      let {edge_data; endpoints = (n1_id, n2_id); _} = get_edge graph edge_id in
      add_edge graph edge_data (
        replace_id old_n_id new_n_id n1_id,
        replace_id old_n_id new_n_id n2_id
      ) |> ignore;
      remove_edge graph edge_id |> ignore
    in
    List.iter (node_edges graph n1_id) ~f:(replace_edge n1_id n_id);
    List.iter (node_edges graph n2_id) ~f:(replace_edge n2_id n_id);
    remove_node graph n1_id |> ignore;
    remove_node graph n2_id |> ignore;
    n_id

  let update_node graph node_id ~new_data =
    Hash_table.set graph.nodes ~key:node_id
      ~data:{(get_node graph node_id) with node_data = new_data}

  let update_edge graph edge_id ~new_data =
    Hash_table.set graph.edges ~key:edge_id
      ~data:{(get_edge graph edge_id) with edge_data = new_data}

  let transpose graph =
    let edge_ids = Hash_table.to_alist graph.edges |> List.map ~f:fst in
    List.iter edge_ids ~f:(fun edge_id ->
        let edge = get_edge graph edge_id in
        let (n1_id, n2_id) = edge.endpoints in
        let new_endpoints = (n2_id, n1_id) in
        let new_edge_id = edge_id_of_endpoints graph new_endpoints in
        Hash_table.set graph.edges ~key:new_edge_id ~data:{edge with endpoints = new_endpoints}
      )

  let node_id_list {nodes; _} =
    nodes
    |> Hash_table.to_alist
    |> List.map ~f:fst

  let node_list {nodes; _} =
    nodes
    |> Hash_table.to_alist
    |> List.map ~f:snd

  let edge_id_list {edges; _} =
    edges
    |> Hash_table.to_alist
    |> List.map ~f:fst

  let edge_list {edges; _} =
    edges
    |> Hash_table.to_alist
    |> List.map ~f:snd

  let iter_nodes graph ~f = List.iter (node_list graph) ~f
  let iter_edges graph ~f = List.iter (edge_list graph) ~f

  let find_node graph ~f = List.find (node_list graph) ~f
  let find_edge graph ~f = List.find (edge_list graph) ~f

  let exists_node graph ~f = Option.is_some (find_node graph ~f)
  let exists_edge graph ~f = Option.is_some (find_edge graph ~f)

  let degree {adj_list; _} node_id = Poly_set.length (Hash_table.find_exn adj_list node_id)
  let in_degree graph node_id = List.length (pred_edges graph node_id) 
  let out_degree graph node_id = List.length (succ_edges graph node_id) 

  let k_color graph k coloring =
    if graph.directed then failwith "Cannot color directed graph"
    else (
      let deg_order =
        node_id_list graph
        |> List.filter ~f:(non (Hash_table.mem coloring))
        |> List.map ~f:(fun n_id -> (n_id, out_degree graph n_id))
        |> List.sort ~compare:(fun (_, deg1) (_, deg2) -> deg1 - deg2)
      in
      let colors = List.init k ~f:ident in
      let graph = copy graph in
      let rec kempe deg_order =
        match deg_order with
        | [] -> None
        | (n_id, deg) :: deg_order' ->
          if deg >= k then Some n_id
          else (
            let neighbors = succ_nodes graph n_id in
            let removed = remove_node graph n_id in
            let ret = kempe deg_order' in
            begin
              match ret with
              | None ->
                let taken = List.map neighbors ~f:(Hash_table.find_exn coloring) in
                let color = List.find_exn colors ~f:(non (List.mem taken ~equal:(=))) in
                Hash_table.set coloring ~key:n_id ~data:color
              | _ -> ()
            end;
            restore_node graph removed |> ignore;
            ret
          )
      in
      match kempe deg_order with
      | None -> `Colored 
      | Some n_id -> `Uncolorable n_id
    )
end

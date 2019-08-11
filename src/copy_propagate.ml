open Cfg
open Common
open Core
open Dataflow
open Graph_lib
open Ir

(* Serialize variable ids for use in Poly_set *)
let serialize t1 t2 = t1 ^ "," ^ t2

(* Deserialize value in Poly_set to a pair of variable ids *)
let deserialize s = 
  let vars = String.split s ~on:',' in 
  match vars with
  | x::y::[] -> (x, y)
  | _ -> List.iter vars ~f:print_endline; failwith "this should never happen"

(* Get a list of all possible variable equality pairs *)
let get_all_equalities (irs : ir_stmt list) =
  let open List in
  (* extract all the MOV instructions from canonical IR stmt list *)
  let mov_stmts = filter irs ~f:(fun ir -> 
      match ir with
      | `IRMove (_, _) -> true
      | _ -> false
    ) in
  (* extract all the unique temps from MOV instructions *)
  let vars = fold mov_stmts ~init:[] ~f:(fun vars mov ->
      match mov with
      | `IRMove (`IRTemp t, _) -> if mem vars t ~equal:(=) then vars else t :: vars
      | _ -> vars)
  in
  cartesian_product vars vars
  |> filter_map ~f:(fun (a, b) -> if a <> b then Some (serialize a b) else None)

(* [map_equalities eqs] converts the Poly_set [eqs] to a 
 * Hash_table for amortized constant lookup. *)
let map_equalities equalities = 
  let map = Hash_table.create (module String) in
  Poly_set.iter equalities
    ~f:(fun eq ->
        let (x, y) = deserialize eq in
        Hash_table.add map ~key:x ~data:y |> ignore;
      );
  map

(* [find_equality eqs use] looks for an equality of the form
 * [use = rhs] in the Hash_table of equalities. Returns (true, rhs) 
 * if an appropriate equality was found, (false, use) otherwise.
 * Applies the transitive property so that if {a = b, b = c}, then uses 
 * of [a] will be mapped to [c]. *)
let find_equality equalities ~use =
  let rec traverse_equalities lhs =
    let ret = Hash_table.find equalities lhs in
    match ret with
    | None -> lhs
    | Some rhs -> traverse_equalities rhs
  in
  traverse_equalities use 

(* [gen n] represents the equalities introduced by node [n] *)
let gen (node : ir_stmt cfg_data) = 
  match node with
  | `Start -> empty ()
  | `Exit -> empty () (* or rv? *)
  | `Other stmt -> 
    let res = empty () in
    match stmt with
    (* equality only introduced in a MOV instruction from one TEMP to another *)
    | `IRMove (`IRTemp t1, `IRTemp t2) -> Poly_set.add res (serialize t1 t2); res
    | _ -> empty ()

(* returns true if temp id has a leading underscore *)
let check_leading_underscore str = String.contains ~pos:0 ~len:1 str '_'

(* [kill n ins] represents the equalities broken by node [n] 
 * and returns ins - kills *)
let kill (node : ir_stmt cfg_data) ins =
  match node with
  | `Start -> empty ()
  | `Exit -> ins
  | `Other stmt ->
    match stmt with
    (* equality only introduced in a MOV instruction from one TEMP to another *)
    | `IRMove (`IRTemp x, `IRTemp y) ->
      Poly_set.filter ins ~f:(fun eq ->
          let (a, b) = deserialize eq in
          not ((a = x && b <> y) || (b = x && a <> y)))
    | `IRMove (`IRTemp x, _) ->
      Poly_set.filter ins ~f:(fun eq ->
          let (a, b) = deserialize eq in 
          not (a = x || b = x))
    | `IRExp (`IRCall _)->
      Poly_set.filter ins ~f:(fun eq ->
          let (a, b) = deserialize eq in 
          not (check_leading_underscore a || check_leading_underscore b))
    | _ -> ins

(* Run the available copies dataflow analysis *)
let analyze_avail_copies (irs : ir_stmt list) = 
  (* generate the CFG for copy propagation from an IR stmt list *)
  let (cfg, nodes, _, _) = 
    generate_cfg 
      ~values:irs
      ~add_succs_func:add_ir_succs
      ~top_func:s_top_func
  in
  let data_store = 
    analyze cfg
      ~eq:Poly_set.equal
      ~dir:`Forward
      ~transfer:(fun {node_data; _} ins -> 
          (* out(n) = gen(n) U (ins - kill(n)) *)
          let gens = gen node_data in
          let ins_kills = kill node_data ins in
          Poly_set.union gens ins_kills
        )
      ~meet:s_meet
      ~top_func:s_top_func
  in
  (cfg, nodes, data_store)

(* Convert CFG to IR stmt list *)
let unwrap_cfg cfg nodes : ir_stmt list = 
  List.filter_map nodes
    ~f:
      (fun {node_id; _} -> 
         let {node_data; _} = Poly_graph.get_node cfg node_id in
         match node_data with
         | `Other stmt -> Some stmt
         | `Start
         | `Exit -> None)

(* Retrieve all the temps used in an expression *)
let get_temps (expr : ir_expr) : id Poly_set.t =
  let rec get_temps e vars = match e with
    | `IRConst _ 
    | `IRName _ -> vars
    | `IRTemp id -> id :: vars
    | `IRBinary (_, e1, e2) -> vars |> get_temps e1 |> get_temps e2
    | `IRMem e -> get_temps e vars
    (* these ir_exprs should not exist in the canonical IR level *)
    | `IRCall _
    | `IRESeq _ -> failwith "this should never happen"
  in
  let vars = get_temps expr [] in
  Poly_set.of_list (module String) vars

(* [replace_use e lhs rhs] uses the equality [lhs = rhs] to substitute 
 * all uses of [lhs] with [rhs] in [e] and returns the new ir_expr. *)
let replace_use e ~lhs ~rhs : ir_expr = 
  let e = string_of_ir_expr e in
  let lhs = string_of_ir_expr (`IRTemp lhs) in
  let rhs = string_of_ir_expr (`IRTemp rhs) in
  let new_e = String.substr_replace_all e ~pattern:lhs ~with_:rhs in
  ir_expr_of_string new_e

(* [substitute_expr e uses eqs] returns the ir_expr after replacing
 * all possible uses with its appropriate equality *)
let substitute_expr e uses equalities : ir_expr = 
  List.fold uses 
    ~init:e 
    ~f:
      (fun e use ->
         let rhs = find_equality equalities ~use in
         replace_use e ~lhs:use ~rhs 
      )

(* Propagate copies in an IR stmt.
 * Calculates use(n) for node [n] that contains ir_stmt [stmt]
 * and replaces all occurrences of the use [x] with [y] if the 
 * equality [x = y] exists in the meet of all [in] edges. *)
let substitute_stmt stmt equalities : ir_stmt = 
  match stmt with
  | `IRMove (`IRMem e1, e2) -> 
    let uses1 = e1 |> get_temps |> Poly_set.to_list in
    let uses2 = e2 |> get_temps |> Poly_set.to_list in
    let e1 = substitute_expr e1 uses1 equalities in
    let e2 = substitute_expr e2 uses2 equalities in
    `IRMove (`IRMem e1, e2)

  | `IRMove (`IRTemp id, e) ->
    let uses = e |> get_temps |> Poly_set.to_list in
    let e = substitute_expr e uses equalities in
    `IRMove (`IRTemp id, e)

  | `IRCJump (e, id1, id2) ->
    let uses = e |> get_temps |> Poly_set.to_list in
    let e = substitute_expr e uses equalities in
    `IRCJump (e, id1, id2)

  | `IRExp (`IRCall (ef, es, n_args, n_rets)) ->
    let es = 
      List.map es ~f:
        (fun e ->
           let uses = e |> get_temps |> Poly_set.to_list in
           substitute_expr e uses equalities
        )
    in
    `IRExp (`IRCall (ef, es, n_args, n_rets))

  | `IRReturn es ->
    let es = 
      List.map es ~f:
        (fun e ->
           let uses = e |> get_temps |> Poly_set.to_list in
           substitute_expr e uses equalities
        )
    in
    `IRReturn es

  | `IRLabel _
  | `IRJump _ -> stmt

  (* IRSeq's should not exist inside IRSeq's in the canonical IR *)
  | `IRSeq _
  | _ -> failwith "this will never happen"

(* Displays the CFG after applying the available copies analysis *)
let display cfg file =
  let cout = Out_channel.create file in
  DDot.output_graph cout
    (ocamlgraph_of_graph cfg
       (fun {node_data; _} ->
          match node_data with
          | `Start -> "START"
          | `Exit -> "EXIT"
          | `Other stmt -> String.strip (string_of_ir_stmt stmt))
       (fun {edge_data; _} ->
          edge_data
          |> Poly_set.to_list
          |> List.map
            ~f:
              (fun s ->
                 let (x, y) = deserialize s in
                 x ^ "=" ^ y
              )
          |> String.concat ~sep:", "
          |> (fun s -> "{" ^ s ^ "}")));
  Out_channel.close cout

let main (module Config : ConfigSpec) (`IRCompUnit (id, funcs, ctors, glob_inits)) : ir_comp_unit =
  (* If specified, don't perform the optimization *)
  if not (Hash_table.find_exn Config.enabled_opts "copy") then
    `IRCompUnit (id, funcs, ctors, glob_inits)
  else (
    debug 2 "propagating copies...";
    let copy_propagate (`IRFunc (id, `IRSeq stmts, n_args, n_rets)) =
      let (cfg, nodes, data_store) = analyze_avail_copies stmts in
      (* tranform cfg here to propagate copies *)
      List.iter nodes ~f:(fun {node_id; node_data} ->
          let equalities =
            node_id
            |> Hash_table.find_exn data_store
            |> map_equalities in
          let (`Other stmt) = node_data in
          let stmt = substitute_stmt stmt equalities in
          let new_data = `Other stmt in
          Poly_graph.update_node cfg node_id ~new_data
        );
      (* If specified, print the CFG after copy propagation *)
      if Hash_table.find_exn Config.optcfg_flags "copy" then
        display cfg (Config.diag_out_path ^ "_" ^ id ^ "_copy.dot");
      `IRFunc (id, `IRSeq (unwrap_cfg cfg nodes), n_args, n_rets)
    in
    `IRCompUnit (id, funcs |> List.map ~f:copy_propagate, ctors, glob_inits)
  )

open Cfg
open Common
open Core
open Dataflow
open Graph_lib
open Ir

(* type node = ir_stmt *)
type el = string Poly_set.t
let eq = Poly_set.equal
let dir = `Forward
let add_succs_func = add_ir_succs

(* Recursively gets subexpressions of an expression *)
let rec downward_exposed_exprs e = 
  let set = empty () in
  match e with
  | `IRBinary (b, e1, e2) -> 
    (if String.is_substring (string_of_ir_expr e) ~substring:"(IRCall"
     then ()
     else Poly_set.add set (string_of_ir_expr (`IRBinary (b, e1, e2)))
    );
    (if String.is_substring (string_of_ir_expr e1) ~substring:"(IRCall"
     then ()
     else Poly_set.union_inplace set (downward_exposed_exprs e1)
    );
    (if String.is_substring (string_of_ir_expr e2) ~substring:"(IRCall"
     then ()
     else Poly_set.union_inplace set (downward_exposed_exprs e2)
    );
    set
  | `IRMem e ->
    if String.is_substring (string_of_ir_expr e) ~substring:"(IRCall"
    then set
    else (
      Poly_set.add set (string_of_ir_expr (`IRMem e));
      Poly_set.union_inplace set (downward_exposed_exprs e);
      set)
  | _ -> set

(* Check if [esub] is a substring of [str] *)
let is_not_subexpression (esub : ir_expr) (str : string) =
  String.is_substring str ~substring:(string_of_ir_expr esub)
  |> not

(* Returns avail with killed expressions removed *)
let kill node_data avail =
  match node_data with
  | `Other s -> (
      let filtered = 
        match s with
        | `IRMove (e1, _) -> Poly_set.filter avail ~f:(is_not_subexpression e1)
        | _ -> avail
      in
      if String.is_substring (string_of_ir_stmt s) ~substring:"(IRCall"
      then 
        Poly_set.filter 
          filtered 
          ~f:(fun a -> String.is_substring a ~substring:"(IRTemp _" |> not)
      else filtered
    )
  | _ -> avail

(* F_n(l) = (l u deexpr(n)) - kill(n) *)
let transfer {node_data; _} l =
  let deexpr =
    match node_data with
    | `Other stmt -> (
        match stmt with
        | `IRMove (_, e)
        | `IRExp e
        | `IRJump e
        | `IRCJump (e, _, _) -> downward_exposed_exprs e
        | `IRSeq _ (* maybe failwith nested irseq is noncanonical *)
        | `IRReturn _
        | `IRLabel _ -> empty ()
      )
    | _ -> empty () 
  in
  Poly_set.union l deexpr
  |> kill node_data

(* DFS for all nodes which use available expr e *)
let rec find_occurrences cfg data_store node_list node_id e exit_id recursion = 
  let uses = Poly_set.create (module String) in
  if node_id = exit_id || List.mem recursion ~equal:(=) node_id
  then uses
  else
    (* let availex = Hashtbl.find_exn data_store node_id in *)
    let availex = match Hashtbl.find data_store node_id with
      | Some a -> a
      | None -> failwith ("cannot find node " ^ node_id)
    in
    let node = match List.find node_list ~f:(fun a -> a.node_id = node_id) with
      | Some a -> a
      | None -> failwith ("Cannot find node " ^ node_id)
    in
    let `Other stmt = node.node_data in
    begin if Poly_set.mem availex e
      then (
        let succ_ids = Poly_graph.succ_nodes cfg node_id in
        List.iter
          succ_ids
          ~f:(fun succ_id ->
              Poly_set.union_inplace
                uses
                (find_occurrences cfg data_store node_list succ_id e exit_id (node_id::recursion))
            )
      )
    end;
    if String.is_substring (string_of_ir_stmt stmt) ~substring:e
    then Poly_set.add uses node_id;
    uses

let unother (`Other o) = o

(* refactors an available expression *)
let refactor (cfg, node_list) node_id (e:string) uses =
  let node_arr = Array.of_list node_list in
  let index_of_src, node_of_src = match Array.findi node_arr ~f:(fun _ i -> node_id = i.node_id) with
    | Some a -> a
    | None -> failwith ("cannot find index of node " ^ node_id)
  in
  let t = make_new_tmp () in
  let tstring = string_of_ir_expr t in
  let temp_move = `IRMove (t, ir_expr_of_string e) in
  Poly_set.iter
    uses
    ~f:(fun use ->
        let index_of_use, node_of_use = match Array.findi node_arr ~f:(fun _ i -> use = i.node_id) with
          | Some a -> a
          | None -> failwith ("cannot find index of use node " ^ node_id)
        in
        let str = string_of_ir_stmt (unother node_of_use.node_data) in
        let newstr = String.substr_replace_all ~pattern:e ~with_:tstring str in
        Array.set node_arr index_of_use {node_of_use with node_data = `Other (ir_stmt_of_string newstr)}
      );
  let str = string_of_ir_stmt (unother node_of_src.node_data) in
  let newstr = String.substr_replace_all ~pattern:e ~with_:tstring str in 
  Array.set node_arr index_of_src {node_of_src with node_data = `Other (ir_stmt_of_string newstr)};
  let new_node_list = Array.to_list node_arr in
  let h, t = List.split_n new_node_list index_of_src in
  let m = [{node_id = (Poly_graph.next_node_id cfg); node_data = `Other temp_move}] in
  List.concat [h;m;t]

(* pick an available expression and refactor it *)
let rec modify (cfg, data_store, node_list) acc node_id exit_id =
  let avail_after_node = Hashtbl.find_exn data_store node_id in
  let generated_by_node = Poly_set.filter
      avail_after_node 
      ~f:(fun a -> Poly_set.mem acc a |> not)
  in
  (* sorted by longest first *)
  let genlist = Poly_set.to_list generated_by_node
                |> List.sort ~compare:(fun a b -> String.(length b - length a))
  in
  (* returns new_list if refactoring has occurred *)
  let rec loop l =
    match l with
    | h::t -> (
        let uses = find_occurrences cfg data_store node_list node_id h exit_id [] in
        if Poly_set.is_empty uses
        then loop t
        else Some (
            let new_list =  refactor (cfg, node_list) node_id h uses in
            List.map new_list ~f:(fun a ->
                unother a.node_data
              )
          )
      )
    | [] -> None
  in
  match loop genlist with
  | Some s -> Some s
  | None -> (
      let rec loop2 l =
        match l with
        | h::t -> (
            match modify (cfg, data_store, node_list) avail_after_node h exit_id with
            | Some s -> Some s
            | None -> loop2 t
          )
        | [] -> None
      in
      loop2 (Poly_graph.succ_nodes cfg node_id)
    )

(* Run CSE on the function *)
let cse_on_func (module Config : ConfigSpec) (`IRFunc (f, `IRSeq stmts, n_args, n_rets)) =
  let (cfg, node_list, start_id, exit_id) =
    Dataflow.generate_cfg
      ~values:stmts
      ~add_succs_func
      ~top_func:s_top_func
  in
  let data_store = Dataflow.analyze cfg
      ~eq:eq
      ~dir:dir
      ~transfer:transfer
      ~meet:s_meet
      ~top_func:s_top_func
  in
  let continueloop = ref true in
  let counter = ref 0 in
  let new_stmts = ref stmts in
  let new_cfg = ref cfg in
  let new_data_store = ref data_store in
  let new_node_list = ref node_list in
  let new_start_id = ref start_id in
  let new_exit_id = ref exit_id in
  (* display_ir_set_graph cfg (Config.diag_out_path ^ "_" ^ f ^ "_cse_debug0.dot"); *)
  (* Keep looping and re-applying CSE until the max number of iterations is hit, or
     there is nothing left to do for CSE *)
  while !continueloop && !counter < 10 do
    incr counter;
    match modify (!new_cfg, !new_data_store, !new_node_list) (empty ()) !new_start_id !new_exit_id with
    | None -> continueloop := false
    | Some n -> (
        let (cfg, node_list, start_id, exit_id) =
          Dataflow.generate_cfg
            ~values:n
            ~add_succs_func
            ~top_func:s_top_func
        in
        let data_store = Dataflow.analyze cfg
            ~eq:eq
            ~dir:dir
            ~transfer:transfer
            ~meet:s_meet
            ~top_func:s_top_func
        in
        new_stmts := n;
        new_data_store := data_store;
        new_node_list := node_list;
        new_start_id := start_id;
        new_exit_id := exit_id
      )
  done;

  (* If specified, print the CFG after optimization *)
  if Hash_table.find_exn Config.optcfg_flags "cse" then (
    let (cfg, _, _, _) =
      Dataflow.generate_cfg
        ~values:!new_stmts
        ~add_succs_func:Dataflow.add_ir_succs
        ~top_func:s_top_func
    in
    display_ir_set_graph cfg (Config.diag_out_path ^ "_" ^ f ^ "_cse.dot")
  );

  `IRFunc (f, `IRSeq (!new_stmts), n_args, n_rets)

let main (module Config : ConfigSpec) (`IRCompUnit (id, funcs, ctors, glob_inits)) : ir_comp_unit =
  (* run_unit_test; *)
  (* If specified, don't perform the optimization *)
  if not (Hash_table.find_exn Config.enabled_opts "cse") then
    `IRCompUnit (id, funcs, ctors, glob_inits)
  else (
    debug 2 "eliminating common subexpressions...";
    `IRCompUnit (id, List.map funcs ~f:(cse_on_func (module Config)), ctors, glob_inits)
  )

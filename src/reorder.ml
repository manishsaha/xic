open Common
open Core
open Ir

type block = ir_stmt Deque.t

let string_of_ir_stmt s = s |> sexp_of_ir_stmt |> Sexp.to_string_hum
let string_of_group g =
  g
  |> List.map ~f:string_of_ir_stmt
  |> String.concat ~sep:"\n"
let string_of_block b =
  b
  |> Deque.to_list
  |> string_of_group
let print_ir_stmt s = Printf.printf "%s\n" (string_of_ir_stmt s)
let print_group g = Printf.printf "%s\n" (string_of_group g)
let print_block b = Printf.printf "%s\n" (string_of_block b)

(* From a list of IR statements [stmts], create a map from labels to
 * basic blocks, where the key is the label that begins the basic block *)
let blocks_of_stmts stmts =
  (* Create an array of statement lists, i.e an array of basic blocks *)
  let group_array =
    List.group stmts ~break:(fun s1 s2 ->
        match (s1, s2) with
        | (`IRJump _, _)
        | (`IRCJump _, _)
        | (`IRReturn _, _)
        | (_, `IRLabel _) -> true
        | _ -> false
      )
    |> Array.of_list
  in

  (* Add a label to the start of each basic block if one doesn't already exist,
   * and store all those labels in [label_array] *)
  let label_array =
    Array.mapi group_array ~f:(fun i group ->
        match List.hd_exn group with
        | `IRLabel l -> l
        | _ ->
          let l = make_new_label () in
          group_array.(i) <- `IRLabel l :: group;
          l
      )
  in

  (* Add a jump to the end of each basic block that doesn't jump/return,
   * representing a fall-through to the next basic block
   * Create a map from labels to basic blocks *)
  let block_map = Hash_table.create (module String) in
  Array.iteri group_array ~f:(fun i group ->
      let block = Deque.create () in
      List.iter group ~f:(Deque.enqueue_back block);
      if i < Array.length group_array - 1 then (
        match Option.value_exn (Deque.peek_back block) with
        | `IRJump _
        | `IRCJump _
        | `IRReturn _ -> ()
        | _ ->
          Deque.enqueue_back block (`IRJump (`IRName (label_array.(i + 1))))
      );
      Hash_table.add_exn block_map ~key:(label_array.(i)) ~data:block
    );
  block_map

(* Create a Control Flow Graph (CFG) from the map of basic blocks [block_map].
 * The CFG maps a label to a list of (1 or 2) labels *)
let cfg_of_blocks block_map =
  let cfg = Hash_table.create (module String) in
  Hash_table.iteri block_map ~f:(fun ~key:label ~data:block ->
      Hash_table.set cfg
        ~key:label
        ~data:(
          match Option.value_exn (Deque.peek_back block) with
          | `IRJump (`IRName l') -> [l']
          | `IRCJump (_, lt, lf) -> [lt; lf]
          | _ -> []
        )
    );
  cfg

(* Return the list of basic blocks visited in a DFS through the [cfg],
 * starting at label [lstart] *)
let trace_of_cfg cfg lstart =
  let visited = Hash_table.create (module String) in
  (* Hash_table.set visited ~key:lend ~data:true; *)
  let trace = ref [] in
  let visit l =
    match Hash_table.add visited ~key:l ~data:true with
    | `Ok -> trace := l :: !trace; true
    | `Duplicate -> false
  in
  let rec dfs l = if visit l then List.iter (Hash_table.find_exn cfg l) ~f:dfs in
  dfs lstart;
  List.rev !trace

let order_by_trace block_map trace =
  let label_array = Array.of_list trace in
  Array.iteri label_array ~f:(fun i l ->
      let block = Hash_table.find_exn block_map l in
      let last_index = Deque.back_index_exn block in
      if i < Array.length label_array - 1 then
        let ln = label_array.(i + 1) in
        begin match Option.value_exn (Deque.peek_back block) with
          (* Remove jump at the end of [block] if it simply jumps
           * to the next block *)
          | `IRJump (`IRName l') ->
            if l' = ln then
              Deque.dequeue_back block |> ignore
          | `IRCJump (e, lt, lf) ->
            if lf <> ln then
              if lt = ln then
                let negate_bop bop =
                  match bop with
                  | `IR_EQ -> `IR_NEQ
                  | `IR_NEQ -> `IR_EQ
                  | `IR_LT -> `IR_GEQ
                  | `IR_LEQ -> `IR_GT
                  | `IR_GT -> `IR_LEQ
                  | `IR_GEQ -> `IR_LT
                  | _ -> failwith "non-negatable bop"
                in
                let is_negatable_bop bop =
                  try negate_bop bop |> ignore; true
                  with _ -> false
                in
                let e' =
                  match e with
                  | `IRBinary (bop, e1, e2) when is_negatable_bop bop ->
                    `IRBinary (negate_bop bop, e1, e2)
                  | `IRBinary (`IR_XOR, e, `IRConst 1L) -> e
                  | _ -> `IRBinary (`IR_XOR, e, ir1)
                in
                let s = `IRCJump (e', lf, lt) in
                Deque.set_exn block last_index s
              else
                Deque.enqueue_back block (`IRJump (`IRName lf));
          | _ -> ()
        end
      else
        (* Add a jump to the last block if it does a CJump, since it will
         * fall through in the false case*)
        begin match Option.value_exn (Deque.peek_back block) with
          | `IRCJump (_, _, lf) ->
            Deque.enqueue_back block (`IRJump (`IRName lf))
          | _ -> ()
        end
    );
  Array.map label_array ~f:(Hash_table.find_exn block_map) |> Array.to_list

let reorder_func (`IRFunc (id, `IRSeq stmts, n_args, n_rets)) =
  let lstart = make_new_label () in
  let stmts = `IRLabel lstart :: stmts in
  let block_map = blocks_of_stmts stmts in
  let cfg = cfg_of_blocks block_map in
  let trace = trace_of_cfg cfg lstart in
  let blocks = order_by_trace block_map trace in
  `IRFunc (id, `IRSeq (blocks |> List.map ~f:Deque.to_list |> List.concat), n_args, n_rets)

let main no_reorder (`IRCompUnit (id, fds, ctors, glob_inits) as lir) =
  if no_reorder then lir else (
    debug 2 "reordering...";
    `IRCompUnit (id, List.map fds ~f:reorder_func, List.map ctors ~f:reorder_func, glob_inits)
  )

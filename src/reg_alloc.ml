open Asm
open Core
open Common

type stack_map = (id, mem) Hash_table.t

(* Takes a list of assembly instructions and produces a mapping from
 * abstract register names to their location on the stack. *)
let generate_stack_map caller_counts asms =
  let stack_map = Hash_table.create (module String) in
  let add_temp t =
    let mem = 
      make_addr ~base:`RBP ~displacement:(to_offset ~-(Hash_table.length stack_map + 1)) ()
    in
    Hash_table.add stack_map ~key:t ~data:mem |> ignore
  in
  let counts_list =
    asms
    |> List.filter_map ~f:(fun asm ->
        match asm with
        | `CALL (_, n_args, n_rets) -> Some (n_args, n_rets)
        | _ -> None
      )
  in
  let (n_args, n_rets) =
    List.fold counts_list ~init:caller_counts
      ~f:(fun (max_args, max_rets) (n_args, n_rets) -> (max max_args n_args, max max_rets n_rets))
  in
  for i = 0 to n_args - 1 do add_temp (nth_arg i) done;
  for i = 0 to n_rets - 1 do add_temp (nth_ret i) done;
  List.iter asms ~f:(fun asm ->
      match asm with
      | #instr as instr ->
        let (written, read) = get_regs_used instr in
        written @ read
        |> List.filter ~f:is_abs_reg
        |> List.map ~f:string_of_reg
        |> List.iter ~f:add_temp
      | _ -> ()
    );
  stack_map

let loc_of_nth_arg stack_map n =
  n
  |> nth_arg
  |> Hash_table.find stack_map
  |> Option.map ~f:(fun l -> (l :> loc))

let loc_of_nth_ret stack_map n =
  n
  |> nth_ret
  |> Hash_table.find stack_map
  |> Option.map ~f:(fun l -> (l :> loc))

(* Given a mapping [stack_map] from abstract register names to memory addresses,
 * and an instruction [instr], generate assembly instructions to perform
 * the operation. *)
let spill_instr stack_map instr =
  let (written, read) = get_regs_used instr in
  let replace_map = Hash_table.create (module String) in
  let regs =
    List.fold (written @ read) ~init:[] ~f:(fun acc reg ->
        if not (List.mem acc reg ~equal:(=)) && is_abs_reg reg then reg :: acc else acc
      )
  in
  List.iteri regs ~f:(fun i reg ->
      let reg_str = string_of_reg reg in
      if Hash_table.mem stack_map reg_str then
        Hash_table.set replace_map ~key:reg_str ~data:spill_regs.(i)
    );
  let load_reg reg =
    let reg_str = string_of_reg reg in
    match Hash_table.find stack_map reg_str with
    | Some mem -> [`MOVQ ((Hash_table.find_exn replace_map reg_str :> loc), (mem :> arg))]
    | None -> []
  in
  let save_reg reg =
    let reg_str = string_of_reg reg in
    match Hash_table.find stack_map reg_str with
    | Some mem -> [`MOVQ ((mem :> loc), (Hash_table.find_exn replace_map reg_str :> arg))]
    | None -> []
  in
  List.(
    concat [
      concat_map read ~f:load_reg;
      [(replace_instr replace_map instr :> asm)];
      concat_map written ~f:save_reg;
    ]
  )

let main caller_counts asms =
  let stack_map = generate_stack_map caller_counts asms in
  let l = Hash_table.length stack_map in
  List.(
    concat [
      [`ENTER (`Const (to_offset (l + l % 2)), `Const 0L)];
      concat_map asms ~f:(fun asm ->
          match asm with
          | `RET -> [`LEAVE; `RET]
          | #instr as instr -> spill_instr stack_map instr
          | _ -> [asm]
        );
    ]
  )

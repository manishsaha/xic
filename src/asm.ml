open Common
open Core

type reg8 = [
  | `AH | `AL | `CH | `CL | `DH | `DL | `BH | `BL | `SPL | `BPL | `SIL | `DIL
  | `R8B | `R9B | `R10B | `R11B | `R12B | `R13B | `R14B | `R15B
] [@@deriving variants]

type reg16 = [
  | `AX | `CX | `DX | `BX | `SP | `BP | `SI | `DI
  | `R8W | `R9W | `R10W | `R11W | `R12W | `R13W | `R14W | `R15W
] [@@deriving variants]

type reg32 = [
  | `EAX | `ECX | `EDX | `EBX | `ESP | `EBP | `ESI | `EDI
  | `R8D | `R9D | `R10D | `R11D | `R12D | `R13D | `R14D | `R15D
] [@@deriving variants]

type reg64 = [
  | `RAX | `RCX | `RDX | `RBX | `RSP | `RBP | `RSI | `RDI
  | `R8 | `R9 | `R10 | `R11 | `R12 | `R13 | `R14 | `R15
] [@@deriving variants]

type abs_reg = [
  | `AbsReg of id
] [@@deriving variants]

type reg = [
  | reg8
  | reg16
  | reg32
  | reg64
  | abs_reg
  | `RIP
]

type label = [
  | `Label of id
]

type const = [
  | `Const of int64
]

type addr = {
  displacement: int64 option;
  base: reg option;
  index: reg option;
  scale: int option;
}

and mem = [
  | `Mem of addr
  | label
]

type loc = [
  | reg
  | mem
]

type arg = [
  | reg
  | const
  | mem
]

type instr = [
  (* moves *)
  | `MOVQ of loc * arg
  | `XCHGQ of loc * loc
  | `LEAQ of reg * mem
  (* control flow *)
  | `TESTQ of loc * loc
  | `CMPQ of loc * loc
  | `JMP of label
  | `JE of label
  | `JNE of label
  | `JG of label
  | `JGE of label
  | `JL of label
  | `JLE of label
  | `CALL of loc * int * int
  | `ENTER of const * const
  | `LEAVE
  | `RET
  | `CQTO
  | `PUSHQ of arg
  | `POPQ of loc
  (* boolean based on condition (google "SETcc") *)
  | `SETE of loc
  | `SETNE of loc
  | `SETL of loc
  | `SETLE of loc
  | `SETG of loc
  | `SETGE of loc
  (* operations *)
  | `ADDQ of loc * arg
  | `SUBQ of loc * arg
  | `IMULQ1 of loc
  | `IMULQ2 of loc * arg
  | `IDIVQ of loc
  | `NEGQ of loc
  | `INCQ of loc
  | `DECQ of loc
  | `ANDQ of loc * arg
  | `ORQ of loc * arg
  | `XORQ of loc * arg
  | `NOTQ of loc
  | `SHRQ of loc * arg
  | `SHLQ of loc * arg
  | `SARQ of loc * arg
] [@@deriving variants]

type asm = [
  | instr
  | label
  | `Comment of string
  | `Globl of id
  | `Ctors
  | `Text
  | `Bss
  | `Align of int
  | `Quad of id
  | `Zero of int
]

type x86 = {
  text: asm list;
  ctors: asm list;
  globals: asm list;
}

let rec string_of_reg reg =
  match reg with 
  | `AbsReg r -> r
  | `RIP -> "rip"
  | #reg8 as r8 -> Variants_of_reg8.to_name r8 |> String.lowercase
  | #reg16 as r16 -> Variants_of_reg16.to_name r16 |> String.lowercase
  | #reg32 as r32 -> Variants_of_reg32.to_name r32 |> String.lowercase
  | #reg64 as r64 -> Variants_of_reg64.to_name r64 |> String.lowercase

and string_of_mem mem =
  match mem with
  | `Mem {displacement; base; index; scale} ->
    let s =
      [
        Option.map base ~f:string_of_reg;
        (match (index, scale) with 
         | (Some r, Some n) ->
           Some (string_of_arg (r :> arg) ^ " * " ^ Int.to_string n)
         | _ -> None);
        Option.map displacement ~f:Int64.to_string;
      ]
      |> List.filter_map ~f:ident
      |> String.concat ~sep:" + "
    in
    "[" ^ s ^ "]"
  | `Label id -> id

and string_of_loc (loc : loc) =
  match loc with
  | #reg as reg -> string_of_reg reg
  | #mem as mem -> string_of_mem mem

and string_of_const (`Const n) = Int64.to_string n

and string_of_arg (arg : arg) =
  match arg with
  | #loc as loc -> string_of_loc loc
  | #const as c -> string_of_const c

and string_of_instr (instr : instr) =
  let name =
    match instr with
    | `IMULQ1 _ 
    | `IMULQ2 _ -> "imulq"
    | _ -> Variants_of_instr.to_name instr |> String.lowercase
  in
  let args =
    match instr with
    | `MOVQ (loc, arg)
    | `SHRQ (loc, arg) 
    | `SHLQ (loc, arg) 
    | `SARQ (loc, arg)
    | `ADDQ (loc, arg) 
    | `SUBQ (loc, arg) 
    | `IMULQ2 (loc, arg)
    | `ANDQ (loc, arg) 
    | `ORQ (loc, arg) 
    | `XORQ (loc, arg) -> [string_of_loc loc ^ ", " ^ string_of_arg arg]
    | `LEAQ (reg, mem) -> [string_of_reg reg ^ ", " ^ string_of_mem mem]
    | `XCHGQ (loc1, loc2)
    | `TESTQ (loc1, loc2)
    | `CMPQ (loc1, loc2) -> [string_of_loc loc1 ^ ", " ^ string_of_loc loc2]
    | `ENTER (c1, c2) -> [string_of_const c1 ^ ", " ^ string_of_const c2]
    | `JMP (`Label l)
    | `JE (`Label l) 
    | `JNE (`Label l)
    | `JG (`Label l) 
    | `JGE (`Label l)
    | `JL (`Label l)
    | `JLE (`Label l) -> [l]
    | `CALL (loc, _, _)
    | `IMULQ1 loc
    | `IDIVQ loc
    | `SETE loc
    | `SETNE loc
    | `SETL loc
    | `SETLE loc
    | `SETG loc
    | `SETGE loc
    | `NEGQ loc
    | `INCQ loc
    | `DECQ loc
    | `NOTQ loc 
    | `POPQ loc -> [string_of_loc loc]
    | `PUSHQ arg -> [string_of_arg arg]
    | `CQTO
    | `LEAVE
    | `RET -> []
  in
  String.concat (name :: args) ~sep:" "

and string_of_asm asm =
  match asm with
  | #instr as instr -> "\t" ^ string_of_instr instr
  | `Label l -> l ^ ":"
  | `Comment c -> "\t// " ^ c
  | `Globl id -> "\t.globl " ^ id
  | `Ctors -> ".section .ctors"
  | `Text -> ".text"
  | `Bss -> ".bss"
  | `Align n -> "\t.align " ^ Int.to_string n
  | `Quad id -> "\t.quad " ^ id
  | `Zero n -> "\t.zero " ^ Int.to_string n

let arg_order : reg array = [|`RDI; `RSI; `RDX; `RCX; `R8; `R9|]
let ret_order : reg array = [|`RAX; `RDX|]
let caller_saved : reg array = [|`RAX; `RCX; `RDX; `RSI; `RDI; `R8; `R9; `R10; `R11|]
let callee_saved : reg array = [|`RBX; `R12; `R13; `R14; `R15|]
let var_regs : reg array = [|`RAX; `RCX; `RDX; `RBX; `RSI; `RDI; `R8; `R9; `R10; `R11; `R12|]
let spill_regs : reg array = [|`R13; `R14; `R15|]
let concrete_regs : reg array = Array.concat [var_regs; spill_regs; [|`RBP; `RSP|]]

let n_arg_regs = Array.length arg_order
let n_ret_regs = Array.length ret_order
let n_caller_saved = Array.length caller_saved
let n_callee_saved = Array.length callee_saved
let n_var_regs = Array.length var_regs

(** [make_addr ?index ?scale ?displacement ?base ()] is the memory address
    [base + (index * scale) + displacement] *)
let make_addr ?index ?scale ?displacement ?base () =
  `Mem {base; index; scale; displacement}

let is_abs_reg r =
  match r with
  | `AbsReg _ -> true
  | _ -> false

let to_offset n = Int64.of_int (8 * n)

let get_regs_from_loc loc =
  match loc with
  | #reg as reg -> [reg]
  | `Mem {base; index; _} ->
    Option.[
      map base ~f:List.return;
      map index ~f:List.return;
    ]
    |> List.filter_map ~f:ident
    |> List.concat
  | _ -> []

(** Get the registers used in an instruction [instr].
    Returns a tuple [(written, read)] where [written] are the register
    that were written to and [read] are registers that were read from
*)
let get_regs_used instr =
  match instr with
  | `MOVQ ((#reg as loc), arg)
  | `LEAQ ((#reg as loc), (#mem as arg)) ->
    (get_regs_from_loc loc, get_regs_from_loc arg)
  | `MOVQ (loc, arg) -> ([], get_regs_from_loc loc @ get_regs_from_loc arg)
  | `SHRQ (loc, arg) 
  | `SHLQ (loc, arg) 
  | `SARQ (loc, arg)
  | `ADDQ (loc, arg) 
  | `SUBQ (loc, arg)
  | `IMULQ2 (loc, arg)
  | `ANDQ (loc, arg) 
  | `ORQ (loc, arg) 
  | `XORQ (loc, arg) ->
    let loc_regs = get_regs_from_loc loc in
    (loc_regs, loc_regs @ get_regs_from_loc arg)
  | `XCHGQ (loc1, loc2) ->
    let regs = get_regs_from_loc loc1 @ get_regs_from_loc loc2 in
    (regs, regs)
  | `TESTQ (loc1, loc2)
  | `CMPQ (loc1, loc2) -> ([], get_regs_from_loc loc1 @ get_regs_from_loc loc2)
  | `IMULQ1 loc -> 
    let regs = get_regs_from_loc loc in
    (`RAX :: `RDX :: regs, `RAX :: regs)
  | `IDIVQ loc ->
    let regs = get_regs_from_loc loc in
    (`RAX :: `RDX :: regs, `RAX :: `RDX :: regs)
  | `CQTO -> ([`RDX], [`RAX])
  | `NEGQ loc
  | `INCQ loc
  | `DECQ loc
  | `NOTQ loc ->
    let regs = get_regs_from_loc loc in
    (regs, regs)
  | `SETE loc
  | `SETNE loc
  | `SETL loc
  | `SETLE loc
  | `SETG loc
  | `SETGE loc
  | `POPQ loc -> (get_regs_from_loc loc, [])
  | `PUSHQ arg -> ([], get_regs_from_loc arg)
  | `CALL (loc, _, _) ->
    List.(
      concat [
        Array.to_list caller_saved;
        (* init n_rets ~f:(fun i -> `AbsReg (nth_ret i)); *)
      ],
      concat [
        Array.to_list arg_order;
        get_regs_from_loc loc;
        (* init n_args ~f:(fun i -> `AbsReg (nth_arg i)); *)
      ]
    )
  | _ -> ([], [])

let set_of_reg_list reg_lst =
  Hash_set.of_list (module String) (List.map reg_lst ~f:string_of_reg)

let find_all_regs asms =
  let regs = Poly_set.create (module String) in
  List.iter asms ~f:(fun asm ->
      match asm with
      | #instr as instr ->
        let (written, read) = get_regs_used instr in
        Poly_set.union_inplace regs (set_of_reg_list written);
        Poly_set.union_inplace regs (set_of_reg_list read)
      | _ -> ()
    );
  regs

let setup_stack (n_args, n_rets) asms =
  let n_arg_stack = max 0 (n_args - n_arg_regs) in
  List.(
    concat [
      (* if skip_save then [] else
       *   init n_callee_saved ~f:(fun i -> `PUSHQ (callee_saved.(i) :> arg)); *)
      init n_args ~f:(fun i ->
          if i < 6 then
            `MOVQ (`AbsReg (nth_arg i), (arg_order.(i) :> arg))
          else
            (* the offset between RBP and the 7th argument in the stack is 2 *)
            `MOVQ (
              `AbsReg (nth_arg i),
              make_addr ~base:`RBP ~displacement:(to_offset (i - n_arg_regs + 2)) ()
            );
        );
      concat_map asms ~f:(fun asm ->
          match asm with
          | `CALL (_, n_args, n_rets) ->
            let n_arg_stack = max 0 (n_args - n_arg_regs) in
            let n_ret_stack = max 0 (n_rets - n_ret_regs) in
            let ret_space = n_ret_stack + if n_ret_stack > 0 then (n_ret_stack + 1) % 2 else 0 in
            concat [
              (* (\* Save caller-saved registers *\)
               *   init n_caller_saved ~f:(fun i -> `PUSHQ (caller_saved.(i) :> arg)); *)
              (* Leave enough space on the stack for multiple return values *)
              if n_rets <= n_ret_regs then [] else
                [
                  `SUBQ (`RSP, `Const (to_offset ret_space));
                  `PUSHQ `RSP;
                ];
              (* Move arguments to registers/stack as specified by the calling conventions *)
              init n_args ~f:(fun i ->
                  if i < n_arg_regs then
                    `MOVQ ((arg_order.(i) :> loc), `AbsReg (nth_arg i))
                  else
                    `PUSHQ (`AbsReg (nth_arg i))
                )
              |> rev;
              [asm];
              (* Pop things from the stack *)
              if n_args <= n_arg_regs then [] else [`ADDQ (`RSP, `Const (to_offset n_arg_stack))];
              if n_rets <= n_ret_regs then [] else 
                [
                  `POPQ `RCX;
                  `ADDQ (`RSP, `Const (to_offset ret_space));
                ];
              init n_rets ~f:(fun i ->
                  let ret_src = 
                    if i < n_ret_regs then
                      (ret_order.(i) :> arg)
                    else
                      make_addr ~base:`RCX ~displacement:(to_offset (i - n_ret_regs)) ()
                  in
                  `MOVQ (`AbsReg (nth_ret i), ret_src);
                )
              |> rev;
              (* if skip_save then [] else
               *   init n_caller_saved ~f:(fun i -> `POPQ (caller_saved.(i) :> loc)) |> rev; *)
            ]
          | `RET ->
            (* the offset between RBP and extra ret address is number of stack args + 2 *)
            let base_mem = make_addr ~base:`RBP ~displacement:(to_offset (n_arg_stack + 2)) () in
            concat [
              (* Use callee saved reg 0 to store the pointer to the beginning of the ret area *)
              if n_rets <= n_ret_regs then [] else
                [`MOVQ (`RCX, (base_mem :> arg))];
              init n_rets ~f:(fun i ->
                  if i < n_ret_regs then
                    `MOVQ ((ret_order.(i) :> loc), `AbsReg (nth_ret i))
                  else
                    let ret_mem =
                      make_addr ~base:`RCX ~displacement:(to_offset (i - n_ret_regs)) ()
                    in
                    `MOVQ (ret_mem, `AbsReg (nth_ret i));
                );
              (* if skip_save then [] else
               *   init n_callee_saved ~f:(fun i -> `POPQ (callee_saved.(i) :> loc))
               *   |> rev; *)
              [`RET];
            ]
          | _ -> [asm]
        );
    ]
  )

let replace_instr replace_map instr =
  let rec replace_reg reg : reg =
    match reg with
    | `AbsReg t when Hash_table.mem replace_map t -> Hash_table.find_exn replace_map t 
    | _ -> reg
  and replace_mem mem : mem =
    match mem with
    | `Mem {displacement; base; index; scale} ->
      `Mem {
        displacement;
        base = Option.map base ~f:replace_reg;
        index = Option.map index ~f:replace_reg;
        scale;
      }
    | _ -> mem
  and replace_loc loc : loc =
    match loc with
    | #reg as reg -> (replace_reg reg :> loc)
    | #mem as mem -> (replace_mem mem :> loc)
  and replace_arg arg : arg =
    match arg with
    | #const -> arg
    | #loc as loc -> (replace_loc loc :> arg)
  in
  match instr with
  | `MOVQ (loc, arg) -> `MOVQ (replace_loc loc, replace_arg arg) 
  | `SHRQ (loc, arg) -> `SHRQ (replace_loc loc, replace_arg arg) 
  | `SHLQ (loc, arg) -> `SHLQ (replace_loc loc, replace_arg arg) 
  | `SARQ (loc, arg) -> `SARQ (replace_loc loc, replace_arg arg)
  | `ADDQ (loc, arg) -> `ADDQ (replace_loc loc, replace_arg arg) 
  | `SUBQ (loc, arg) -> `SUBQ (replace_loc loc, replace_arg arg) 
  | `IMULQ2 (loc, arg) -> `IMULQ2 (replace_loc loc, replace_arg arg)
  | `ANDQ (loc, arg) -> `ANDQ (replace_loc loc, replace_arg arg) 
  | `ORQ (loc, arg) -> `ORQ (replace_loc loc, replace_arg arg)
  | `XORQ (loc, arg) -> `XORQ (replace_loc loc, replace_arg arg)
  | `LEAQ (reg, mem) -> `LEAQ (replace_reg reg, replace_mem mem)
  | `XCHGQ (loc1, loc2) -> `XCHGQ (replace_loc loc1, replace_loc loc2)
  | `TESTQ (loc1, loc2) -> `TESTQ (replace_loc loc1, replace_loc loc2)
  | `CMPQ (loc1, loc2) -> `CMPQ (replace_loc loc1, replace_loc loc2)
  | `CALL (loc, n_args, n_rets) -> `CALL (replace_loc loc, n_args, n_rets)
  | `IMULQ1 loc -> `IMULQ1 (replace_loc loc)
  | `IDIVQ loc -> `IDIVQ (replace_loc loc)
  | `SETE loc -> `SETE (replace_loc loc)
  | `SETNE loc -> `SETNE (replace_loc loc)
  | `SETL loc -> `SETL (replace_loc loc)
  | `SETLE loc -> `SETLE (replace_loc loc)
  | `SETG loc -> `SETG (replace_loc loc)
  | `SETGE loc -> `SETGE (replace_loc loc)
  | `NEGQ loc -> `NEGQ (replace_loc loc)
  | `INCQ loc -> `INCQ (replace_loc loc)
  | `DECQ loc -> `DECQ (replace_loc loc)
  | `NOTQ loc -> `NOTQ (replace_loc loc) 
  | `POPQ loc -> `POPQ (replace_loc loc)
  | `PUSHQ arg -> `PUSHQ (replace_arg arg)
  | _ -> instr

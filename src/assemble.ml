open Asm
open Cfg
open Common
open Core
open Ir

let _MAIN_ = "_Imain_paai"

(* is_reg l is true is [l] is a (non-ret, non-arg) register; false otherwise *)
let is_reg (t : id) : bool =
  (String.is_prefix t ~prefix:"_RET" = false) &&
  (String.is_prefix t ~prefix:"_ARG" = false)

(* [is_int32 i] returns true if [i] is a signed 32-bit constant; false otherwise *)
let is_int32 i =
  Option.is_some (Int64.to_int32 i)

(* [effective_addr_of_expr e] takes an IR expr [e] and returns [`Mem mem] if it
 *  is an effective address. Raises an error otherwise
 *  (Note: This doesn't account for associativity/commutativity of
 *  base_reg, index_reg, scale, or displacement)
 *  Possible permutations of base_reg + index_reg*scale + displacement:
 *  1. (base/index)reg
 *  2. base_reg + index_reg
 *  3a. index_reg*scale (where scale = 1, 2, 4, 8)
 *  3b. index_reg*scale (where scale = 3, 5, 9)
 *  4. base_reg + index_reg*scale (where scale = 1, 2, 4, 8)
 *  5. displacement
 *  6a. (base/index)reg + displacement
 *  6b. (base/index)reg - displacement
 *  7a. base_reg + index_reg + displacement
 *  7b. base_reg + index_reg - displacement
 *  8a. index_reg*scale + displacement (where scale = 1, 2, 4, 8)
 *  8b. index_reg*scale - displacement (where scale = 1, 2, 4, 8)
 *  9a. base_reg + index_reg*scale + displacement (where scale = 1, 2, 4, 8)
 *  9b. base_reg + index_reg*scale - displacement (where scale = 1, 2, 4, 8) *)
let effective_addr_of_expr (e : ir_expr) : mem =
  let make_addr_helper ?index ?scale ?disp ?base () =
    (* Precondition: [t] is not "_RET*" or "_ARG*" *)
    let reg_of_temp_helper t : reg =
      assert (not (String.is_prefix t ~prefix:"_RET"));
      assert (not (String.is_prefix t ~prefix:"_ARG"));
      `AbsReg t
    in
    make_addr
      ?base:(Option.map base  ~f:reg_of_temp_helper)
      ?index:(Option.map index ~f:reg_of_temp_helper)
      ?scale:(Option.map scale ~f:Int64.to_int_exn)
      ?displacement:disp
      ()
  in
  match e with
  (* 1 *)
  | `IRTemp base
    when is_reg base ->
    make_addr_helper ~base ()
  (* 2 *)
  | `IRBinary (`IR_ADD, `IRTemp base, `IRTemp index)
    when is_reg base && is_reg index ->
    make_addr_helper ~base ~index ~scale:1L ()
  (* 3a *)
  | `IRBinary (`IR_MUL, `IRTemp index, `IRConst scale)
    when is_reg index &&
         List.mem [1L; 2L; 4L; 8L] scale ~equal:(=) ->
    make_addr_helper ~index ~scale ()
  (* 3b *)
  | `IRBinary (`IR_MUL, `IRTemp index, `IRConst scale)
    when is_reg index &&
         List.mem [3L; 5L; 9L] scale ~equal:(=) ->
    make_addr_helper ~base:index ~index ~scale:Int64.(scale - 1L) ()
  (* 4 *)
  | `IRBinary (
      `IR_ADD,
      `IRTemp base,
      `IRBinary (`IR_MUL, `IRTemp index, `IRConst scale))
    when is_reg base && is_reg index &&
         List.mem [1L; 2L; 4L; 8L] scale ~equal:(=) ->
    make_addr_helper ~base ~index ~scale ()
  (* 5 *)
  | `IRConst disp when is_int32 disp ->
    make_addr_helper ~disp ()
  (* 6a *)
  | `IRBinary (`IR_ADD, `IRTemp base, `IRConst disp)
    when is_reg base && is_int32 disp ->
    make_addr_helper ~base ~disp ()
  (* 6b *)
  | `IRBinary (`IR_SUB, `IRTemp base, `IRConst disp)
    when is_reg base && is_int32 disp ->
    make_addr_helper ~base ~disp:Int64.(-disp) ()
  (* 7a *)
  | `IRBinary (
      `IR_ADD,
      `IRTemp base,
      `IRBinary (`IR_ADD, `IRTemp index, `IRConst disp)
    )
    when is_reg base && is_reg index && is_int32 disp ->
    make_addr_helper ~base ~index ~scale:1L ~disp ()
  (* 7b *)
  | `IRBinary (
      `IR_ADD,
      `IRTemp base,
      `IRBinary (`IR_SUB, `IRTemp index, `IRConst disp)
    )
    when is_reg base && is_reg index && is_int32 disp ->
    make_addr_helper ~base ~index ~scale:1L ~disp:Int64.(-disp) ()
  (* 8a *)
  | `IRBinary (
      `IR_ADD,
      `IRBinary (`IR_MUL, `IRTemp index, `IRConst scale),
      `IRConst disp
    )
    when is_reg index && is_int32 disp &&
         List.mem [1L; 2L; 4L; 8L] scale ~equal:(=) ->
    make_addr_helper ~index ~scale ~disp ()
  (* 8b *)
  | `IRBinary (
      `IR_SUB,
      `IRBinary (`IR_MUL, `IRTemp index, `IRConst scale),
      `IRConst disp
    )
    when is_reg index && is_int32 disp &&
         List.mem [1L; 2L; 4L; 8L] scale ~equal:(=) ->
    make_addr_helper ~index ~scale ~disp:Int64.(-disp) ()
  (* 9a *)
  | `IRBinary (
      `IR_ADD,
      `IRTemp base,
      `IRBinary (
        `IR_ADD,
        `IRBinary (`IR_MUL, `IRTemp index, `IRConst scale),
        `IRConst disp)
    )
    when is_reg base && is_reg index && is_int32 disp &&
         List.mem [1L; 2L; 4L; 8L] scale ~equal:(=) ->
    make_addr_helper ~base ~index ~scale ~disp ()
  (* 9b *)
  | `IRBinary (
      `IR_ADD,
      `IRTemp base,
      `IRBinary (
        `IR_SUB,
        `IRBinary (`IR_MUL, `IRTemp index, `IRConst scale),
        `IRConst disp)
    )
    when is_reg base && is_reg index && is_int32 disp &&
         List.mem [1L; 2L; 4L; 8L] scale ~equal:(=) ->
    make_addr_helper ~base ~index ~scale ~disp:Int64.(-disp) ()
  (* Default *)
  | _ -> failwithf "expr is not an effective address: %s" (string_of_ir_expr e) ()

(* [is_effective_addr e] is true if [e] is some effective address; false otherwise *)
let is_effective_addr (e : ir_expr) : bool =
  try effective_addr_of_expr e |> ignore; true
  with _ -> false

(* [asm_of_ir_expr e r] is the assembly for IR expr [e], where the result
 * of [e] is to be stored in [r] *)
let rec asm_of_ir_expr (e : ir_expr) (#reg as r : [> reg ]) : asm list =
  let setcc_of_binop op =
    match op with
    | `IR_EQ -> sete
    | `IR_NEQ -> setne
    | `IR_LT -> setl
    | `IR_GT -> setg
    | `IR_LEQ -> setle
    | `IR_GEQ -> setge
    | _ -> failwith "Cannot get SETcc of binop"
  in
  let is_setcc_binop op =
    try setcc_of_binop op |> ignore; true
    with _ -> false
  in
  match e with
  | `IRConst n -> [`MOVQ (r, `Const n)]
  | `IRTemp t -> [`MOVQ (r, (`AbsReg t :> arg))]
  | _ when is_effective_addr e -> [`LEAQ (r, effective_addr_of_expr e)]

  (* use bit shifting for speedy power-of-2 multiplication *)
  | `IRBinary (`IR_MUL, e, `IRConst i) when is_pow2 i ->
    asm_of_ir_expr e r @ [`SHLQ (r, `Const (floor_log2 i))]
  | `IRBinary (`IR_MUL, `IRConst i, e) when is_pow2 i ->
    asm_of_ir_expr e r @ [`SHLQ (r, `Const (floor_log2 i))]

  (* use LEA shortcuts for speedy 2/3/4/5/8/9 multiplication *)
  | `IRBinary (`IR_MUL, e, `IRConst i) when (i = 3L || i = 5L || i = 9L) ->
    asm_of_ir_expr e r
    @ [`LEAQ (r, make_addr ~base:r ~index:r ~scale:(Int64.to_int_exn i - 1) ())]
  | `IRBinary (`IR_MUL, `IRConst i, e) when (i = 3L || i = 5L || i = 9L) ->
    asm_of_ir_expr e r
    @ [`LEAQ (r, make_addr ~base:r ~index:r ~scale:(Int64.to_int_exn i - 1) ())]

  (* use normal instructions for non speedy normal arithmetic *)
  | `IRBinary (`IR_ADD, e1, e2) ->
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r';
      [`ADDQ (r, r')];
    ]
  | `IRBinary (`IR_SUB, e1, e2) ->
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r';
      [`SUBQ (r, r')];
    ]
  | `IRBinary (`IR_MUL, e1, e2) ->
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r';
      [`IMULQ2 (r, r')];
    ]
  (* conveniently, IMULQ with 1 argument is 128-bit multiplication! *)
  | `IRBinary (`IR_HMUL, e1, e2) ->
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r';
      [
        `MOVQ (`RAX, r);
        `IMULQ1 r';
        `MOVQ (r, `RDX);
      ];
    ]
  (* we use XOR 1 as NOT -- should be only case but here's the full logic just in case *)
  | `IRBinary (`IR_XOR, e1, e2) ->
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r';
      [`XORQ (r, r')];
    ]

  | `IRBinary (`IR_LSHIFT as op, e1, e2)
  | `IRBinary (`IR_RSHIFT as op, e1, e2)
  | `IRBinary (`IR_ARSHIFT as op, e1, e2) ->
    let instr_of_shift =
      function
      | `IR_LSHIFT -> shlq
      | `IR_RSHIFT -> shrq
      | `IR_ARSHIFT -> sarq
    in
    let r2 = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r2;
      [(instr_of_shift op) (r, r2)];
    ]

  (* use setcc for clean boolean operations *)
  | `IRBinary (op, e1, e2) when is_setcc_binop op ->
    let r1 = make_new_abs_reg () in
    let r2 = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r1;
      asm_of_ir_expr e2 r2;
      [
        `XORQ (r, r);
        `CMPQ (r1, r2);
        (setcc_of_binop op) r;
      ];
    ]

  (* use right shift to avoid division *)
  | `IRBinary (`IR_DIV, e, `IRConst i) when is_pow2 i ->
    asm_of_ir_expr e r @ [`SARQ (r, `Const (floor_log2 i))]
  (* but not always *)
  | `IRBinary (`IR_DIV, e1, e2) ->
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r';
      [
        `MOVQ (`RAX, r);
        `IDIVQ r';
        `MOVQ (r, `RAX);
      ];
    ]
  (* wow, IDIVQ puts the remainder in RDX! *)
  | `IRBinary (`IR_MOD, e1, e2) ->
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r;
      asm_of_ir_expr e2 r';
      [
        `MOVQ (`RAX, r);
        `IDIVQ r';
        `MOVQ (r, `RDX);
      ];
    ]

  (* use labels to represent names *)
  | `IRName id -> [`LEAQ (r, `Label id)]
  | `IRMem (`IRName id) -> [`MOVQ (r, `Label id)]
  | `IRMem e -> 
    let r' = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e r';
      [`MOVQ (r, make_addr ~base:r' ())];
    ]

  (* calling a name directly doesn't need an extra temp to store the function expr *)
  | `IRCall (`IRName id, es, n_args, n_rets) ->
    List.(
      concat [
        concat_mapi es ~f:(fun i e ->
            asm_of_ir_expr e (`AbsReg (nth_arg i))
          );
        [`CALL (`Label id, n_args, n_rets)];
      ]
    )
  (* but this does in case of mem accesses *)
  | `IRCall (ef, es, n_args, n_rets) ->
    let rf = make_new_abs_reg () in
    List.(
      concat [
        asm_of_ir_expr ef rf;
        concat_mapi es ~f:(fun i e ->
            asm_of_ir_expr e (`AbsReg (nth_arg i))
          );
        [`CALL (rf, n_args, n_rets)];
      ]
    )

  | _ -> failwithf "impossible expr: %s" (string_of_ir_expr e) ()

(* [asm_of_ir_stmt s] is the assembly for IR stmt [s] *)
let asm_of_ir_stmt (s : ir_stmt) : asm list =
  match s with
  (* makin moves *)
  | `IRMove (`IRTemp t, e) ->
    let r = make_new_abs_reg () in
    asm_of_ir_expr e r @ [`MOVQ (`AbsReg t, r)]
  | `IRMove (`IRMem e, `IRTemp t) ->
    let r = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e r;
      [`MOVQ (make_addr ~base:r (), `AbsReg t)];
    ]
  | `IRMove (`IRMem e1, e2) ->
    let r1 = make_new_abs_reg () in
    let r2 = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e1 r1;
      asm_of_ir_expr e2 r2;
      [`MOVQ (make_addr ~base:r1 (), r2)];
    ]

  (* hops *)
  | `IRJump (`IRName l) -> [`JMP (`Label l)]
  | `IRCJump (`IRBinary (`IR_XOR, e, `IRConst 1L), l, _) ->
    let r = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e r;
      [
        `TESTQ (r, r);
        `JE (`Label l);
      ]
    ]
  | `IRCJump (`IRBinary (op, e1, e2), l, _) ->
    let r1 = make_new_abs_reg () in
    let r2 = make_new_abs_reg () in
    let l = `Label l in
    List.concat [
      asm_of_ir_expr e1 r1;
      asm_of_ir_expr e2 r2;
      [
        `CMPQ (r1, r2);
        begin
          match op with
          | `IR_EQ  -> `JE  l
          | `IR_NEQ -> `JNE l
          | `IR_LT  -> `JL  l
          | `IR_LEQ -> `JLE l
          | `IR_GT  -> `JG  l
          | `IR_GEQ -> `JGE l
          | _ -> failwithf "impossible cjump: %s" (string_of_ir_stmt s) ()
        end;
      ];
    ]
  | `IRCJump (e, l, _) ->
    let r = make_new_abs_reg () in
    List.concat [
      asm_of_ir_expr e r;
      [
        `TESTQ (r, r);
        `JNE (`Label l);
      ]
    ]
  | `IRLabel l -> [`Label l]
  | `IRExp e -> asm_of_ir_expr e (make_new_abs_reg ())
  | `IRReturn es ->
    List.(
      let rs = map es ~f:(fun _ -> make_new_abs_reg ()) in
      concat [
        map2_exn es rs ~f:asm_of_ir_expr |> concat;
        mapi rs ~f:(fun i r -> `MOVQ (`AbsReg (nth_ret i), r));
        [`RET];
      ]
    )
  | _ -> failwithf "impossible stmt: %s" (string_of_ir_stmt s) ()

(* Make the register-allocated asm list linker-ready by fixing the SETcc instructions to
 * take the proper-size (and zeroed) registers, extending RAX to 128-bit before IMULQ/IDIVQ,
 * and pushing/popping used callee-saved registers *)
let postprocess asms =
  let reg8_of_loc loc =
    match loc with
    | `RAX -> `AL
    | `RBX -> `BL
    | `RCX -> `CL
    | `RDX -> `DL
    | `RSP -> `SPL
    | `RBP -> `BPL
    | `RSI -> `SIL
    | `RDI -> `DIL
    | `R8 -> `R8B
    | `R9 -> `R9B
    | `R10 -> `R10B
    | `R11 -> `R11B
    | `R12 -> `R12B
    | `R13 -> `R13B
    | `R14 -> `R14B
    | `R15 -> `R15B
    | _ -> failwithf "cannot make reg8 of loc %s" (string_of_loc loc) ()
  in
  let all_reg_set = find_all_regs asms in
  let regs_to_save =
    callee_saved
    |> Array.filter ~f:(compose (Poly_set.mem all_reg_set) string_of_reg)
    |> Array.to_list
  in
  List.(
    concat [
      concat_map asms ~f:(fun asm ->
          match asm with
          | `SETE loc -> [`MOVQ (loc, `Const 0L); `SETE (reg8_of_loc loc)]
          | `SETNE loc -> [`MOVQ (loc, `Const 0L); `SETNE (reg8_of_loc loc)]
          | `SETL loc -> [`MOVQ (loc, `Const 0L); `SETL (reg8_of_loc loc)]
          | `SETLE loc -> [`MOVQ (loc, `Const 0L); `SETLE (reg8_of_loc loc)]
          | `SETG loc -> [`MOVQ (loc, `Const 0L); `SETG (reg8_of_loc loc)]
          | `SETGE loc -> [`MOVQ (loc, `Const 0L); `SETGE (reg8_of_loc loc)]
          | `IMULQ1 _
          | `IDIVQ _ -> [`CQTO; asm]
          | `ENTER _ ->
            concat [
              [asm];
              (if ((List.length regs_to_save) % 2) = 1
               then [`SUBQ (`RSP, `Const 8L)]
               else []);
              map regs_to_save ~f:(fun reg -> `PUSHQ (reg :> arg))
            ]
          | `LEAVE ->
            concat [
              map regs_to_save ~f:(fun reg -> `POPQ (reg :> loc)) |> rev;
              [asm];
            ]
          | _ -> [asm]
        );
    ]
  )

(* Generate assembly of a function declaration and body *)
let asm_of_ir_func_decl (module Config : ConfigSpec) (`IRFunc (f, `IRSeq ss, n_args, n_rets)) =
  let do_reg_alloc = Hash_table.find_exn Config.enabled_opts "reg" in
  let final_asms =
    List.(
      ss
      |> concat_map ~f:asm_of_ir_stmt
      |> concat_map
        ~f:(fun asm -> if !Common._DEBUG_ >= 1 then [`Comment (string_of_asm asm); asm] else [asm])
      |> setup_stack (n_args, n_rets)
      |> (if do_reg_alloc then Reg_alloc2.main f else Reg_alloc.main (n_args, n_rets))
      |> postprocess
      |> cons (`Label f)
      |> (fun asms -> if f = _MAIN_ then `Globl _MAIN_ :: asms else asms)
    )
  in
  if Hash_table.find_exn Config.optcfg_flags "reg" then (
    let (cfg, _, _, _) =
      Dataflow.generate_cfg
        ~values:final_asms
        ~add_succs_func:Dataflow.add_asm_succs
        ~top_func:(fun () -> Poly_set.create (module String))
    in
    display_cfg cfg (Config.diag_out_path ^ "_" ^ f ^ "_reg.dot")
  );
  final_asms

(* Print x86 assembly instructions to [cout] *)
let print_assembly asms cout =
  Printf.fprintf cout ".intel_syntax noprefix\n";
  asms
  |> List.map ~f:string_of_asm
  |> String.concat ~sep:"\n"
  |> Printf.fprintf cout "%s\n%!"

let main (`IRCompUnit (_, fds, ctors, glob_inits)) (module Config : ConfigSpec) =
  let do_reg_alloc = Hash_table.find_exn Config.enabled_opts "reg" in
  debug 2 "assembling with %s register allocation..."
    (if do_reg_alloc then "advanced" else "trivial");
  List.(
    concat [
      [
        `Text;
        `Globl "_Imain_paai";
      ];
      concat_map fds ~f:(asm_of_ir_func_decl (module Config));
      concat_map ctors ~f:(asm_of_ir_func_decl (module Config));
      [
        `Ctors;
        `Align 8;
      ];
      map ctors ~f:(fun (`IRFunc (f, _, _, _)) -> `Quad f);
      [`Bss];
      concat_map glob_inits ~f:(fun (id, size) ->
          [
            `Align 8;
            `Globl id;
            `Label id;
            `Zero size;
          ]
        );
    ];
  )

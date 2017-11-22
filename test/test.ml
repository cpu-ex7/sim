open OUnit
open Simulator
open Program
open Sim
open Asm
open Core
open Operand
open Operator

let parse_test () =
  assert_equal
    (parse "add $t1, $zero, $zero\n")
    [|(OpAdd, (Reg 9, Reg 0, Reg 0))|];

  assert_equal
    (parse "addi $a0, $a0, -1\n")
    [|(OpAddi, (Reg 4, Reg 4, Imm (-1)))|];

  assert_equal
    (parse "add $a2, $a3, 123\naddi $t3, $t3, $t4\n")
    [|(OpAdd, (Reg 6, Reg 7, Imm 123));
      (OpAddi, (Reg 11, Reg 11, Reg 12))|];
  ()

let compile_test () =
  assert_equal
    (Sim.file "sum.input"; !Program.g_program)
    [|(OpAdd,  (Reg 9, Reg 0, Reg 0));
      (OpSlti, (Reg 8, Reg 4, Imm 1));
      (OpBne,  (Reg 8, Reg 0, Dest 6));
      (OpAdd,  (Reg 9, Reg 9, Reg 4));
      (OpAddi, (Reg 4, Reg 4, Imm (-1)));
      (OpJump, (Dest 1, Empty, Empty));
      (OpAdd,  (Reg 2, Reg 9, Reg 0));
      (OpHalt, (Empty, Empty, Empty))|];
  ()

let exec_test () =
  assert_equal
    (exec_func "sum.input" "sum" 10)
    55;
  ()

let asm_test () =
  (Sim.file "sum.input");
  assert_equal
    (assembly_string ())
    "00000000000000000100100000100000
00101000100010000000000000000001
00010101000000000000000000000100
00000001001001000100100000100000
00100000100001001111111111111111
00001000000000000000000000000001
00000001001000000001000000100000\n\n"

let memory_test () =
  Sim.file "fibtable.input";
  assert_equal
    (exec_all () |> ignore;
     Sim.dump_memory_alist ())
    [(4160, 987);
     (4156, 610);
     (4152, 377);
     (4148, 233);
     (4144, 144);
     (4140, 89);
     (4136, 55);
     (4132, 34);
     (4128, 21);
     (4124, 13);
     (4120, 8);
     (4116, 5);
     (4112, 3);
     (4108, 2);
     (4104, 1);
     (4100, 1);
     (4096, 0)]

let instruction_test () =
  let assert_equal = assert_equal ?printer:(Some string_of_int) in
  Sim.string
    "li  $t0, 7
     li  $t1, 3
     and $t0, $t0, $t1";
  assert_equal
    (exec_all ()).reg.(Operand.regnum_of_string "$t0")
    3;

  Sim.string
    "li  $t0, 6
     li  $t1, 1
     or $t0, $t0, $t1";
  assert_equal
    (exec_all ()).reg.(Operand.regnum_of_string "$t0")
    7;

  Sim.string
    "li  $t0, 7
     li  $t1, 1
     xor $t0, $t0, $t1";
  assert_equal
    (exec_all ()).reg.(Operand.regnum_of_string "$t0")
    6;

  Sim.string
    "li  $t0, 7
     li  $t1, 1
     nor $t0, $t0, $t1";
  assert_equal
    (exec_all ()).reg.(Operand.regnum_of_string "$t0")
    ~-8;

  Sim.string
    "li  $t0, 2
     li  $t1, 16
     sub $t0, $t0, $t1";
  assert_equal
    (exec_all ()).reg.(Operand.regnum_of_string "$t0")
    ~-14;
  ()

let suite =
  "suite" >::: [
    "Parser" >:: parse_test;
    "Compile" >:: compile_test;
    "Execution" >:: exec_test;
    "Assembly" >:: asm_test;
    "Memory" >:: memory_test;
    "Instruction" >:: instruction_test;
  ]

let _ = run_test_tt_main suite

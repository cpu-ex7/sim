open OUnit
open Simulator
open Sim
open Asm
open OpSyntax

let parse_test () =
  assert_equal
    (parse "add $t1, $zero, $zero\n")
    [|(OpAdd, [|Reg 9; Reg 0; Reg 0|])|];

  assert_equal
    (parse "addi $a0, $a0, -1\n")
    [|(OpAddi, [|Reg 4; Reg 4; Imm (-1)|])|];


  assert_equal
    (parse "add $a2, $a3, 123\naddi $t3, $t3, $t4\n")
    [|(OpAdd, [|Reg 6; Reg 7; Imm 123|]);
      (OpAddi, [|Reg 11; Reg 11; Reg 12|])|];
  ()

let compile_test () =
  assert_equal
    (compile_file "sum.input"; !Program.g_lines)
    [|(OpAdd,  [|Reg 9; Reg 0; Reg 0|]);
      (OpSlti, [|Reg 8; Reg 4; Imm 1|]);
      (OpBne,  [|Reg 8; Reg 0; Dest 6|]);
      (OpAdd,  [|Reg 9; Reg 9; Reg 4|]);
      (OpAddi, [|Reg 4; Reg 4; Imm (-1)|]);
      (OpJump, [|Dest 1|]);
      (OpAdd,  [|Reg 2; Reg 9; Reg 0|]);
      (OpHalt, [||])|];
  ()

let exec_test () =
  assert_equal
    (exec_func "sum.input" "sum" 10)
    55;
  ()

let asm_test () =
  (compile_file "sum.input");
  assert_equal
    (assembly_string ())
    "00000000000000000100100000100000
00101000100010000000000000000001
00010101000000000000000000000100
00000001001001000100100000100000
00100000100001001111111111111111
00001000000000000000000000000001
00000001001000000001000000100000\n\n"

let suite =
  "suite" >::: [
    "Parser" >:: parse_test;
    "Compile" >:: compile_test;
    "Execution" >:: exec_test;
    "Assembly" >:: asm_test;
  ]

let _ = run_test_tt_main suite

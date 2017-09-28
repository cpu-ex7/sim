open OUnit
open Simulator
open Sim
open Main
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
    (compile_file "sum.input")
    [|(OpAdd,  [|Reg 9; Reg 0; Reg 0|]);
      (OpSlti, [|Reg 8; Reg 4; Imm 1|]);
      (OpBne,  [|Reg 8; Reg 0; Dest 6|]);
      (OpAdd,  [|Reg 9; Reg 9; Reg 4|]);
      (OpAddi, [|Reg 4; Reg 4; Imm (-1)|]);
      (OpJump, [|Dest 2|]);
      (OpAdd,  [|Reg 2; Reg 9;  Reg 0|]);
      (OpHalt, [||])|];
  ()

let exec_test () =
  assert_equal
    (exec_func "sum.input" "sum" 10)
    55;
  ()

let suite =
  "suite" >::: [
    "Parser" >:: parse_test;
  ]

let _ = run_test_tt_main suite

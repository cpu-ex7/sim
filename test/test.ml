open OUnit
open Simulator
open Sim
open Main
open OpSyntax

let parse_test () =
  assert_equal
    (parse "add $t1, $zero, $zero\n")
    { ops = [|(OpAdd, [|Reg 9; Reg 0; Reg 0|])|];
      labels = [] };

  assert_equal
    (parse "addi $a0, $a0, -1\n")
    { ops = [|(OpAddi, [|Reg 4; Reg 4; Imm (-1)|])|];
      labels = [] };

  assert_equal
    (parse "add $a2, $a3, 123\naddi $t3, $t3, $t4\n")
    { ops = [|(OpAdd, [|Reg 6; Reg 7; Imm 123|]);
              (OpAddi, [|Reg 11; Reg 11; Reg 12|])|];
      labels = [] };

  assert_equal
    (parse_file "sum.input")
    {ops =[|(OpAdd,  [|Reg 9; Reg 0; Reg 0|]);
            (OpSlti, [|Reg 8; Reg 4; Imm 1|]);
            (OpBne,  [|Reg 8; Reg 0; Dest "sum_exit"|]);
            (OpAdd,  [|Reg 9; Reg 9; Reg 4|]);
            (OpAddi, [|Reg 4; Reg 4; Imm (-1)|]);
            (OpJump, [|Dest "suminner"|]);
            (OpAdd,  [|Reg 2; Reg 9;  Reg 0|]);
            (OpHalt, [||])|];
     labels = [("sum_exit", 6); ("suminner", 1); ("sum", 0)] };
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

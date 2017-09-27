open OUnit
open Sim
open Main
open OpSyntax

let parse_test () =
  assert_equal
    (parse "add $t1, $zero, $zero")
    [(OpAdd, [|Reg 9; Reg 0; Reg 0|])];

  assert_equal
    (parse "addi $a0, $a0, -1")
    [(OpAddi, [|Reg 4; Reg 4; Imm (-1)|])];

  assert_equal
    (parse "add $a2, $a3, 123\naddi $t3, $t3, $t4")
    [(OpAdd, [|Reg 6; Reg 7; Imm 123|]);
     (OpAddi, [|Reg 11; Reg 11; Reg 12|])];

  ()

let suite =
  "suite" >::: [
    "Parser" >:: parse_test;
  ]

let _ = run_test_tt_main suite

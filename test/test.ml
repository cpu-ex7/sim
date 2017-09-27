open OUnit
open Simulator
open Sim
open Main
open OpSyntax

let parse_test () =
  assert_equal
    (parse_file "parse.input")
    [(OpAdd, [|Reg 6; Reg 7; Imm 123|]);
     (OpAddi, [|Reg 11; Reg 11; Reg 12|]);
     (OpSlti, [|Reg 2; Reg 26; Imm (-9)|])]
  ;
  ()

let suite =
  "suite" >::: [
    "Parser" >:: parse_test;
  ]

let _ = run_test_tt_main suite

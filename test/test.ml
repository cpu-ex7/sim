open OUnit
open Sim
open Main
open OpSyntax

let parse_test () =
  assert_equal
    (parse "add $t1, $zero, $zero")
    [OpAdd [9; 0; 0]];

  ()

let suite =
  "suite" >::: [
    "Parser" >:: parse_test;
  ]

let _ = run_test_tt_main suite

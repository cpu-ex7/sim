open OUnit
open Simulator
open Operand
open Core

let file_test () =
  App.reset_all ();
  App.load_file "fib.input";
  App.set_args [10];
  assert_equal
    ?printer:(Some string_of_int)
    (App.execute ()).reg.(Operand.regnum_of_string "$v0")
    55;

  App.reset_all ();
  App.load_file "fib.input";
  let core = App.execute () in
  assert_equal
    ?printer:(Some (fun (x, y, z) -> Format.sprintf "%d %d %d" x y z))
    (core.reg.(regnum_of_string "$v0"),
     core.reg.(regnum_of_string "$sp"),
     core.reg.(regnum_of_string "$fp"))
    (55, 4096, 4096);
  ()

let instruction_test () =
  let assert_equal = assert_equal ?printer:(Some string_of_int) in

  App.reset_all ();
  App.load_string
    "addi  $t0, $t0, 7
     addi  $t1, $t1, 3
     and   $t0, $t0, $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    3;

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   6
     addi  $t1, $zero,   1
     or   $t0, $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    7;

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   7
     addi  $t1, $zero,   1
     xor $t0,  $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    6;

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   7
     addi  $t1, $zero,   1
     nor $t0,  $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    ~-8;

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   2
     addi  $t1, $zero,  16
     sub $t0,  $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    ~-14;
  ()

let suite =
  "suite" >::: [
    "File" >:: file_test;
    "Instruction" >:: instruction_test;
  ]

let _ = run_test_tt_main suite

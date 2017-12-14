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

  App.reset_all ();
  App.set_input "hoge";
  App.load_string "
    read_char $zero
    read_char $at
    read_char $v0
    read_char $v1";
  let core = App.execute () in
  assert_equal
    (core.reg.(regnum_of_string "$zero"),
     core.reg.(regnum_of_string "$at"),
     core.reg.(regnum_of_string "$v0"),
     core.reg.(regnum_of_string "$v1"))
    (104, 111, 103, 101);

  App.reset_all ();
  App.load_string "
    addi $v0, $zero, 104
    print_char $v0
    addi $v0, $zero, 111
    print_char $v0
    addi $v0, $zero, 103
    print_char $v0
    addi $v0, $zero, 101
    print_char $v0";
  ignore (App.execute ());
  (* 出力が"hoge"ならOK *)

  ()

let suite =
  "suite" >::: [
    "File" >:: file_test;
    "Instruction" >:: instruction_test;
  ]

let _ = run_test_tt_main suite

open Int32
open OUnit
open Simulator
open Operand
open Core

let file_test () =
  App.reset_all ();
  App.load_file "fib.input";
  App.set_args [10];
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$v0")
    (of_int 55);

  App.reset_all ();
  App.load_file "fib.input";
  let core = App.execute () in
  assert_equal
    (core.reg.(regnum_of_string "$v0"),
     core.reg.(regnum_of_string "$sp"),
     core.reg.(regnum_of_string "$fp"))
    (of_int 55, of_int 4096, of_int 4096);
  ()

let instruction_test () =

  App.reset_all ();
  App.load_string
    "addi  $t0, $t0, 7
     addi  $t1, $t1, 3
     and   $t0, $t0, $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    (of_int 3);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   6
     addi  $t1, $zero,   1
     or   $t0, $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    (of_int 7);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   7
     addi  $t1, $zero,   1
     xor $t0,  $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    (of_int 6);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   7
     addi  $t1, $zero,   1
     nor $t0,  $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    (of_int ~-8);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero,   2
     addi  $t1, $zero,  16
     sub $t0,  $t0,   $t1";
  assert_equal
    (App.execute ()).reg.(Operand.regnum_of_string "$t0")
    (of_int ~-14);

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
    (of_int 104, of_int 111, of_int 103, of_int 101);

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

  App.reset_all ();
  App.load_string
    (Printf.sprintf"
       ori  $v1, $zero, %s
       sw   $v1, 0($zero)
       lwc1 $f0, 0($zero)
       addf $f1, $f0, $f0
     " (Int32.to_string (Int32.bits_of_float 12345.6789)));
  let core = App.execute () in
  assert_equal
    ~printer:(fun (x, y) -> Printf.sprintf "%f, %f" x y)
    (core.freg.(0), core.freg.(1))
    (12345.6789 |> Int32.bits_of_float |> Int32.float_of_bits,
     12345.6789 |> Int32.bits_of_float |> Int32.float_of_bits |> ( *. ) 2.0);
  ()

let suite =
  "suite" >::: [
    "File" >:: file_test;
    "Instruction" >:: instruction_test;
  ]

let _ = run_test_tt_main suite

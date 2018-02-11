open Int32
open OUnit
open Simulator
open Operand
open Core
open Float32

let file_test () =
  App.reset_all ();
  App.load_file "fib.input";
  App.set_args [10];
  assert_equal
    (App.execute_noexcept ()).reg.(Operand.regnum_of_string "$v0")
    (of_int 55);

  App.reset_all ();
  App.load_file "fib.input";
  let core = App.execute_noexcept () in
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
    ~msg: "ori"
    (App.execute_noexcept ()).reg.(Operand.regnum_of_string "$t0")
    (of_int 3);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero, 6
     addi  $t1, $zero, 1
     or    $t0, $t0, $t1";
  assert_equal
    ~msg:"or"
    (App.execute_noexcept ()).reg.(Operand.regnum_of_string "$t0")
    (of_int 7);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero, 7
     addi  $t1, $zero, 1
     xor   $t0, $t0, $t1";
  assert_equal
    ~msg:"xor"
    (App.execute_noexcept ()).reg.(Operand.regnum_of_string "$t0")
    (of_int 6);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero, 7
     addi  $t1, $zero, 1
     nor   $t0, $t0, $t1";
  assert_equal
    ~msg:"nor"
    (App.execute_noexcept ()).reg.(Operand.regnum_of_string "$t0")
    (of_int ~-8);

  App.reset_all ();
  App.load_string
    "addi  $t0, $zero, 2
     addi  $t1, $zero, 16
     sub   $t0, $t0, $t1";
  assert_equal
    ~msg:"sub"
    (App.execute_noexcept ()).reg.(Operand.regnum_of_string "$t0")
    (of_int ~-14);

  App.reset_all ();
  App.set_input_string "hoge";
  App.load_string "
    read_char $zero
    read_char $at
    read_char $v0
    read_char $v1";
  let core = App.execute_noexcept () in
  assert_equal
    ~msg: "read_char"
    (core.reg.(regnum_of_string "$zero"),
     core.reg.(regnum_of_string "$at"),
     core.reg.(regnum_of_string "$v0"),
     core.reg.(regnum_of_string "$v1"))
    (of_int 104, of_int 111, of_int 103, of_int 101);

  App.reset_all ();
  App.set_input_string "hoge";
  App.load_string "read_word $zero";
  let core = App.execute_noexcept () in
  assert_equal
    ~msg: "read_word"
    core.reg.(regnum_of_string "$zero")
    (* 0b1101000110111111001111100101 = 'e'::'g'::'o'::'h' *)
    (of_int 0b1100101110011111011111101000);

  App.reset_all ();
  App.set_input_file "hoge.input";
  App.load_string "
    read_char $zero
    read_char $t0
    read_char $t1
    read_char $t2
    read_char $t3
    read_char $t4";
  let core = App.execute_noexcept () in
  assert_equal
    ~msg: "set_input_file()"
    (core.reg.(regnum_of_string "$zero"),
     core.reg.(regnum_of_string "$t0"),
     core.reg.(regnum_of_string "$t1"),
     core.reg.(regnum_of_string "$t2"),
     core.reg.(regnum_of_string "$t3"),
     core.reg.(regnum_of_string "$t4"))
    (104l, 111l, 10l, 103l, 10l, 101l);

  App.reset_all ();
  App.load_string "
    addi $v0, $zero, 105
    print_char $v0
    addi $v0, $zero, 116
    print_char $v0
    addi $v0, $zero, 32
    print_char $v0
    addi $v0, $zero, 119
    print_char $v0
    addi $v0, $zero, 111
    print_char $v0
    addi $v0, $zero, 114
    print_char $v0
    addi $v0, $zero, 107
    print_char $v0
    addi $v0, $zero, 115
    print_char $v0
    addi $v0, $zero, 33
    print_char $v0";
  ignore (App.execute_noexcept ());
  (* 出力が"it works!"ならOK *)

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "lui  $v1, %s
        ori  $v1, $v1, %s
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        addf $f1, $f0, $f0"
       (upper_bits_of_float 12345.6789 |> to_string)
       (lower_bits_of_float 12345.6789 |> to_string));
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"addf"
    (core.freg.(0), core.freg.(1))
    (12345.6789 |> Int32.bits_of_float,
     24691.3578 |> Int32.bits_of_float);

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "lui  $v1, %s
        ori  $v1, $v1, %s
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        abs $f1, $f0, $f0"
       (upper_bits_of_float ~-.12345.6789 |> to_string)
       (lower_bits_of_float ~-.12345.6789 |> to_string));
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"abs"
    (core.freg.(0), core.freg.(1))
    (-12345.6789 |> Int32.bits_of_float,
     12345.6789 |> Int32.bits_of_float);

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "lui  $v1, %s
        ori  $v1, $v1, %s
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        abs $f1, $f0, $f0"
       (upper_bits_of_float 12345.6789 |> to_string)
       (lower_bits_of_float 12345.6789 |> to_string));
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"abs"
    (core.freg.(0), core.freg.(1))
    (12345.6789 |> Int32.bits_of_float,
     12345.6789 |> Int32.bits_of_float);

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "ori  $v1, $v1, 12345
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        cvtsw $f1, $f0, $f0");
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"cvtsw"
    (core.freg.(0), core.freg.(1))
    (12345l,
     12345.0 |> Int32.bits_of_float);

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "lui  $v1, %s
        ori  $v1, $v1, %s
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        roundwfmt $f1, $f0, $f0"
       (upper_bits_of_float 12345.45 |> to_string)
       (lower_bits_of_float 12345.45 |> to_string));
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"roundwfmt"
    (core.freg.(0), core.freg.(1))
    (12345.45 |> Int32.bits_of_float,
     12345.0 |> Int32.bits_of_float);

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "lui  $v1, %s
        ori  $v1, $v1, %s
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        roundwfmt $f1, $f0, $f0"
       (upper_bits_of_float 0.5 |> to_string)
       (lower_bits_of_float 0.5 |> to_string));
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"roundwfmt"
    (core.freg.(0), core.freg.(1))
    (0.5 |> Int32.bits_of_float,
     1.0 |> Int32.bits_of_float);

  App.reset_all ();
  App.load_string
    "ori $t1, $zero, 3
     jalr $t1
     halt
     halt";
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"jalr"
    (!(core.pc), core.reg.(regnum_of_string "$ra"))
    (3l, 2l);

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "lui  $v1, %s
        ori  $v1, $v1, %s
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        mulf $f1, $f0, $f0
        gtf  $f0, $f1
        bc1t hoge
        halt
      hoge:
        halt"
       (upper_bits_of_float 10.0 |> to_string)
       (lower_bits_of_float 10.0 |> to_string));
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"bct"
    !(core.pc)
    7l;

  App.reset_all ();
  App.load_string
    (Printf.sprintf
       "lui  $v1, %s
        ori  $v1, $v1, %s
        sw   $v1, 0($zero)
        lwc1 $f0, 0($zero)
        mulf $f1, $f0, $f0
        gtf  $f0, $f1
        bc1f hoge
        halt
      hoge:
        halt"
       (upper_bits_of_float 10.0 |> to_string)
       (lower_bits_of_float 10.0 |> to_string));
  let core = App.execute_noexcept () in
  assert_equal
    ~msg:"bcf"
    !(core.pc)
    8l;

  ()

let special_label () =
  App.reset_all ();
  App.load_string
    "    j @
     @:
         j @@
     @@:
         j @@@
     @@@:
         addi $v0, $zero, 42
         halt";
  let core = App.execute_noexcept () in
  assert_equal
    core.reg.(regnum_of_string "$v0")
    42l;
  ()



let suite =
  "suite" >::: [
    "File" >:: file_test;
    "Instruction" >:: instruction_test;
    "SpecialDLabel" >:: special_label;
  ]

let _ = run_test_tt_main suite

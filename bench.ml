open Simulator
open Core
open Operand

let () =
  Sim.file "test/sum.input";
  !g_core.reg.(regnum_of_string "$a0") <- 200000000;
  let core =  Sim.exec_all () in
  print_int !(core.count);
  print_newline ()

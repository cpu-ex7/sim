open Simulator
open Core
open Operand

let () =
  App.load_file "test/sum.input";
  App.set_args [20000000];
  let core =  App.execute () in
  print_int !(core.count);
  print_newline ()

open Simulator
open Sim

let () =
  Sim.file "test/sum.input";
  let core = (empty_core ()) in
  core.reg.(OpSyntax.regnum_of_string "$a0") <- 20000000;
  let core =  (Sim.exec_all core) in
  print_int !(core.count);
  print_newline ()

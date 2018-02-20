open Simulator
open Core
open Operand

let () =
  App.load_file "test/min-rt.s";
  App.set_input_file "test/contest";
  let core = App.execute_noexcept () in
  Printf.eprintf "%d\n" !(core.count);
  App.print_log ()

open Simulator
open Core
open Operand

let () =
  App.load_file "../min-caml/cpuex/min-rt.s";
  App.set_input_file "/home/admin2/Desktop/cserver-com/convert_from_sld/sld/contest";;
  let core =  App.execute () in
  ()

open JsExt
open Sim

let num_regs = 32
let input_area = createElement document "textarea"
let run_button = createElement document "input"
let reg_buttons =
  ArrayLabels.init num_regs ~f:(fun _ -> createElement document "textarea")

let fetch core =
  ArrayLabels.iteri
    ~f:(fun i button ->
      ArrayLabels.set core.reg i (int_of_string (get button "value")))
    reg_buttons

let writeback core =
  ArrayLabels.iteri
    ~f:(fun i button -> set button "textContent" (ArrayLabels.get core.reg i))
    reg_buttons

let run () =
  let program = Sim.compile (get input_area "textContent") in
  let core = Sim.empty_core () in
  fetch core;
  ignore (Sim.exec_all ~core:core program);
  writeback core;
  ()

let init_widgets () =
  set input_area "cols" "80";
  set input_area "rows" "30";
  set input_area "textContent"
    "sum:
  add $t1, $zero, $zero
suminner:
  slti $t0, $a0, 1
  bne $t0, $zero, sum_exit
  add $t1, $t1, $a0
  addi $a0, $a0, -1
  j suminner
sum_exit:
  add $v0, $t1, $zero
  halt
";

  set run_button "type" "button";
  set run_button "value" "run";
  set run_button "onclick" run;

  ArrayLabels.iter
    ~f:(fun button ->
      set button "textContent" "0";
      set button "cols" "5";
      set button "rows" "1")
    reg_buttons;
  ()

let place_widgets () =
  insert_before current_script run_button;
  insert_before current_script (createElement document "br");

  insert_before current_script input_area;
  insert_before current_script (createElement document "br");

  for i=0 to num_regs  - 1 do
    insert_before current_script (ArrayLabels.get reg_buttons i)
  done;
  ()

let _ =
  init_widgets ();
  place_widgets ();
  ()

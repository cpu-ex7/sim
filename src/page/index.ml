open JsExt
open Sim
open OpSyntax

let num_regs = 32
let input_area = Textarea.create ()
let run_button = Button.create ()
let reg_textareas = ArrayLabels.init num_regs ~f:(fun _ -> Textarea.create ())
let asm_button = Button.create ()
let asm_output_area = Textarea.create ()

let (||>) v f = f v; v

let fetch core =
  ArrayLabels.iteri
    ~f:(fun i button ->
      ArrayLabels.set core.reg i (int_of_string (Textarea.get_text button)))
    reg_textareas

let writeback core =
  ArrayLabels.iteri
    ~f:(fun i button ->
      Textarea.set_text (string_of_int (ArrayLabels.get core.reg i)) button)
    reg_textareas

let run () =
  Sim.compile (input_area |> Textarea.get_text);
  let core = Sim.empty_core () in
  fetch core;
  ignore (Sim.exec_all core);
  writeback core;
  ()

let assmeble () =
  Sim.compile (input_area |> Textarea.get_text);
  asm_output_area |> Textarea.set_text (Asm.assembly_string ());
  ()

let init_widgets () =
  input_area
  ||> Textarea.set_cols 80
  ||> Textarea.set_rows 30
  |> Textarea.set_text
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

  run_button
  ||> Button.set_text "run"
  |> Button.set_onclick run;

  ArrayLabels.iter
    ~f:(fun tarea ->
      tarea
      ||> Textarea.set_text "0"
      ||> Textarea.set_cols 20
      |> Textarea.set_rows 1)
    reg_textareas;

  asm_button
  ||> Button.set_text "assembly"
  |> Button.set_onclick assmeble;

  asm_output_area
  ||> Textarea.set_readonly
  ||> Textarea.set_text "assembly output here"
  ||> Textarea.set_cols 35
  |> Textarea.set_rows 30;

  ()

let rec range ?(start=0) ?(step=1) m =
  if start >= m then []
  else start :: range ~start:(start + step) ~step:(step) m

let place_widgets () =
  Document.just_put run_button;
  Document.just_put asm_button;
  Document.just_put (Document.new_line ());

  Document.just_put input_area;
  Document.just_put asm_output_area;
  Document.just_put (Document.new_line ());

  let trs =
    ListLabels.map ~f:(fun i ->
      Tr.create ()
      ||> Tr.add_cell (Span.create () ||> Span.set_text (string_of_regnum i))
      ||> Tr.add_cell (ArrayLabels.get reg_textareas i))
      (range num_regs) in
  Table.from_trs trs |> Document.just_put;
  ()

let _ =
  init_widgets ();
  place_widgets ();
  ()

open Core
open Int32

(* シミュレーターの状態 *)
type state = {
  program : Program.t ref;
  verified : Verify.t ref;
  label : Label.t ref;
  core : Core.t ref;
}

(* グローバルの状態 *)
let global = ref {
  program = ref [||];
  verified = ref [||];
  label = ref [];
  core = ref (Core.empty ());
}

let reset_all () =
  global := {
    program = ref [||];
    verified = ref [||];
    label=ref [];
    core= ref (Core.empty ());
  }

let reset_core () = !global.core := Core.empty ()

let label _ = !(!global.label)
let program _ = !(!global.program)
let verified _ = !(!global.verified)
let core _ = !(!global.core)
let count _ = !(!(!global.core).count)
let pc _ = to_int !(!(!global.core).pc)
let log _ = (core ()).log

(* プログラムを一行だけ実行する *)
let execute_n n =
  for i=0 to n-1 do
    Sim.execute (core ()) (verified ()).(pc ());
    (* (log ()) := (Log.update (verified ()).(pc ()) !(log ())) *)
  done;
  (core ())

(* 終了するまでプログラムを実行する/例外を出さない *)
let execute_noexcept () =
  (try
     while true
     do
       (* ログを更新 *)
       (core ()).count := !((core ()).count) + 1;
       (* 1行だけ実行する *)
       ignore (execute_n 1)
     done;
   with _ -> ());
  core ()

(* 終了するまでプログラムを実行する/例外を出す *)
let execute () =
  while true
  do
    (* ログを更新 *)
    (core ()).count := !((core ()).count) + 1;
    (* 1行だけ実行する *)
    ignore (execute_n 1)
  done;
  core ()

(* レジスタのロード関連 *)
(* 引数用レジスタにnumsをセットする *)
let set_args = Core.set_args !global.core
let set_register_name = Core.set_register_name !global.core
let set_register_idx = Core.set_register_idx !global.core
let dump_memory = Core.dump_memory !global.core
let dump_memory_alist = Core.dump_memory_alist !global.core

(* プログラムのロード関連 *)
let label_parse_file filename =
  Lexing.from_channel (open_in filename)
  |> LabelParser.toplevel LabelLexer.main

let label_parse_string str =
  Lexing.from_string str
  |> LabelParser.toplevel LabelLexer.main

let parse_string str =
  Lexing.from_string str
  |> Parser.toplevel Lexer.main

let parse_file filename =
  Lexing.from_channel (open_in filename)
  |> Parser.toplevel Lexer.main

let load_file filename =
  Label.global := label_parse_file filename;
  !global.label := !Label.global;
  !global.program := parse_file filename;
  !global.verified := Verify.f (program ())

let load_string str =
  (* 最後に改行がないとまずいので... *)
  let str = str ^ "\n" in
  Label.global := label_parse_string str;
  !global.label := !Label.global;
  !global.program := parse_string str;
  !global.verified := Verify.f (program ())

(* コアの状態をリセットすることなくstrを実行する *)
(* デバッグ用 *)
let execute_string str =
  load_string str;
  (core global).pc := of_int 0;
  execute ()

let print_assembly () =
  Asm.print_assembly (verified global)

let string_of_assembly () =
  Asm.string_of_assembly (verified global)

let set_input_string str =
  (core global).input_index := 0;
  (core global).input_string := str

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done;
    ""
  with End_of_file ->
    close_in chan;
    let str = List.fold_left (fun x y -> x ^ "\n" ^ y) "" @@ List.rev !lines in
    String.sub str 1 ((String.length str) - 1)

let set_input_file filename =
  (core global).input_index := 0;
  (core global).input_string := read_file filename

let print_label _ = Label.print_label (label ())
let print_program _ = Program.print_program_with_label (program ()) (label ())
let print_verified _ = Verify.print_verified (verified ())
let print_core _ = Core.print_core (core ())
let print_log _ = Log.print_log !(log ())

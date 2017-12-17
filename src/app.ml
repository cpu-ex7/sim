open Core
open Int32

(* シミュレーターの状態 *)
type state = {
  program : Verify.t ref;
  label   : Label.t ref;
  core    : Core.t ref;
}

(* グローバルの状態 *)
let g = ref {
    program = ref [||];
    label=ref [];
    core= ref (Core.empty ());
  }

let reset_all () =
  g := {
    program = ref [||];
    label=ref [];
    core= ref (Core.empty ());
  }

let reset_core () = !g.core := Core.empty ()

let program g = !(!g.program)
let core g = !(!g.core)
let count g = !(!(!g.core).count)
let pc g = to_int !(!(!g.core).pc)

(* プログラムを一行だけ実行する *)
let execute_one_line () =
  Sim.execute (core g) (program g).(pc g)

(* 終了するまでプログラムを実行する *)
let execute () =
  (try while true
     do
       (* ログを更新 *)
       (core g).count := !((core g).count) + 1;
       (* 1行だけ実行する *)
       execute_one_line ()
     done with _ -> ()); (* Intex_error or ExecutionEnd *)
  (core g)

(* レジスタのロード関連 *)
(* 引数用レジスタにnumsをセットする *)
let set_args = Core.set_args !g.core
let set_register_name = Core.set_register_name !g.core
let set_register_idx = Core.set_register_idx !g.core
let dump_memory = Core.dump_memory !g.core
let dump_memory_alist = Core.dump_memory_alist !g.core

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
  ParserArgs.label := label_parse_file filename;
  !g.label := !ParserArgs.label;
  !g.program := !(Verify.f (parse_file filename))

let load_string str =
  (* 最後に改行がないとまずいので... *)
  let str = str ^ "\n" in
  ParserArgs.label := label_parse_string str;
  !g.label := !ParserArgs.label;
  !g.program := !(Verify.f (parse_string str))

let print_assembly () =
  Asm.print_assembly (program g)

let string_of_assembly () =
  Asm.string_of_assembly (program g)

let set_input str =
  (core g).input_index := 0;
  (core g).input_string := str

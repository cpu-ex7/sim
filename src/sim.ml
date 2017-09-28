open OpSyntax

exception ExecutionEnd

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)
let parse_file filename =
  Lexing.from_channel (open_in filename)
  |> Parser.toplevel Lexer.main

let label_parse_file filename =
  Lexing.from_channel (open_in filename)
  |> LabelParser.toplevel LabelLexer.main
let label_parse str =
  Lexing.from_string str
  |> LabelParser.toplevel LabelLexer.main

let compile_file filename =
  let label = label_parse_file filename in
  Label.update_label label;
  parse_file filename
let compile str =
  let label = label_parse str in
  Label.update_label label;
  parse_file str

type core = {
  pc : int ref;
  reg : int array;
}

let empty_core () = { pc = ref 0; reg = Array.make 32 0}
let incr core = core.pc := !(core.pc) + 1

let rget core i = core.reg.(i)
let rset core i n = core.reg.(i) <- n

let exec_add core operands =
  match operands with
  | [|Reg i; Reg j; Reg k|] ->
      rset core i (rget core j + rget core k);
      incr core
  | _ -> failwith "add: bad operands"

let exec_addi core operands =
  match operands with
  | [|Reg i; Reg j; Imm k|] ->
      rset core i (rget core j + k);
      incr core
  | _ -> failwith "addi: bad operands"

let exec_slti core operands =
  match operands with
  | [|Reg i; Reg j; Imm k|] ->
      rset core i (if rget core j < k then 1 else 0);
      incr core
  | _ -> failwith "addi: bad operands"

let exec_jump core operands =
  match operands with
  | [|Dest i|] -> core.pc := i
  | _ -> failwith "jump: bad operands"

let exec_bne core operands =
  match operands with
  | [|Reg i; Reg j; Dest k|] ->
      core.pc := if rget core i <> rget core j then k else !(core.pc) + 1
  | _ -> failwith "bne: bad operands"

let exec_oneline core line =
  match line with
  | OpAdd, operands -> exec_add core operands
  | OpAddi, operands -> exec_addi core operands
  | OpSlti, operands -> exec_slti core operands
  | OpJump, operands -> exec_jump core operands
  | OpBne, operands -> exec_bne core operands
  | OpHalt, _ -> raise ExecutionEnd

let exec_once lines core =
  exec_oneline core lines.(!(core.pc));
  core

let exec_all ?(core = empty_core ()) lines =
  (try while true do
       exec_oneline  core lines.(!(core.pc))
     done with ExecutionEnd -> ());
  core

(* filename内の1引数1返り値の関数を実行する *)
let exec_func filename funcname arg =
  let lines = compile_file filename in
  let start = List.assoc funcname !(Label.g_label) in
  let core = empty_core () in
  core.pc := start;
  core.reg.(4) <- arg;
  ignore (exec_all ~core:core lines);
  core.reg.(2)

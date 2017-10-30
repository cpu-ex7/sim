open OpSyntax

exception ExecutionEnd

module IH = Hashtbl.Make
    (struct
      type t = int
      let hash x = x
      let equal: int -> int -> bool = (=)
    end)

type core = {
  pc : int ref;
  reg : int array;
  mem : int IH.t;
  count : int ref;
}

let empty_core () = { pc = ref 0; reg = Array.make 32 0; mem = IH.create 256; count = ref 0 }
let incr core = core.pc := !(core.pc) + 1

let rget core i = core.reg.(i)
let rset core i n = core.reg.(i) <- n

(* Program.g_linesのオペランドがあっているかを確認し、
   簡略化したプログラムをg_lines_verifiedに格納する。 *)
let verify_operands () =
  Program.lnum := Array.length !(Program.g_lines);
  Program.g_lines_verified := Array.make !(Program.lnum) (OpHalt, 0,0,0);
  for index = 0 to !(Program.lnum) - 1 do
    match !(Program.g_lines).(index) with
    (* 算術命令 *)
    | OpAdd,  (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpAdd,  i, j, k)
    | OpSub,  (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpSub,  i, j, k)
    | OpAddi, (Reg i, Reg j, Imm k)  -> !(Program.g_lines_verified).(index) <- (OpAddi, i, j, k)
    (* 論理命令 *)
    | OpAnd,  (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpAnd,  i, j, k)
    | OpOr,   (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpOr,   i, j, k)
    | OpNor,  (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpNor,  i, j, k)
    | OpXor,  (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpXor,  i, j, k)
    (* シフト命令 *)
    | OpSll,  (Reg i, Reg j, Imm k)  -> !(Program.g_lines_verified).(index) <- (OpSll,  i, j, k)
    | OpSllv, (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpSllv, i, j, k)
    | OpSrl,  (Reg i, Reg j, Imm k)  -> !(Program.g_lines_verified).(index) <- (OpSrl,  i, j, k)
    | OpSrlv, (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpSrlv, i, j, k)
    | OpSra,  (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpSra,  i, j, k)
    | OpSrav, (Reg i, Reg j, Imm k)  -> !(Program.g_lines_verified).(index) <- (OpSrav, i, j, k)
    (* 制御命令 *)
    | OpSlt,  (Reg i, Reg j, Reg k)  -> !(Program.g_lines_verified).(index) <- (OpSlt,  i, j, k)
    | OpSlti, (Reg i, Reg j, Imm k)  -> !(Program.g_lines_verified).(index) <- (OpSlti, i, j, k)
    | OpJump, (Dest i, Empty, Empty) -> !(Program.g_lines_verified).(index) <- (OpJump, i, 0, 0)
    | OpJal,  (Dest i, Empty, Empty) -> !(Program.g_lines_verified).(index) <- (OpJal,  i, 0, 0)
    | OpJr,   (Reg i, Empty, Empty)  -> !(Program.g_lines_verified).(index) <- (OpJr,   i, 0, 0)
    | OpBne,  (Reg i, Reg j, Dest k) -> !(Program.g_lines_verified).(index) <- (OpBne,  i, j, k)
    | OpBeq,  (Reg i, Reg j, Dest k) -> !(Program.g_lines_verified).(index) <- (OpBeq,  i, j, k)
    | OpHalt, _                      -> !(Program.g_lines_verified).(index) <- (OpHalt, 0, 0, 0)
    (* メモリ命令 *)
    | OpLi,   (Reg i, Imm j, Empty)  -> !(Program.g_lines_verified).(index) <- (OpLi,   i, j, 0)
    | OpLui,  (Reg i, Imm j, Empty)  -> !(Program.g_lines_verified).(index) <- (OpLui,  i, j, 0)
    | OpLw,   (Reg i, RelReg (d, from), Empty) -> !(Program.g_lines_verified).(index) <- (OpLw, i, d, from)
    | OpSw,   (Reg i, RelReg (d, from), Empty) -> !(Program.g_lines_verified).(index) <- (OpSw, i, d, from)
    | op, _ -> failwith ("bad operand: " ^ (List.assoc op OpSyntax.op_alist_rev))
  done

let exec_oneline core =
  core.count := !(core.count) + 1;
  if !(core.pc) >= !(Program.lnum) then raise ExecutionEnd
  else
    let line = !Program.g_lines_verified.(!(core.pc)) in
    match line with
    (* 算術命令 *)
    | OpAdd, i, j, k ->
        rset core i (rget core j + rget core k);
        incr core
    | OpSub, i, j, k ->
        rset core i (rget core j - rget core k);
        incr core
    | OpAddi, i, j, k ->
        rset core i (rget core j + k);
        incr core
    (* 論理命令 *)
    | OpAnd, i, j, k ->
        rset core i (rget core j land rget core k);
        incr core
    | OpOr, i, j, k ->
        rset core i (rget core j lor rget core k);
        incr core
    | OpNor, i, j, k ->
        rset core i (lnot (rget core j lor rget core k));
        incr core
    | OpXor, i, j, k ->
        rset core i (rget core j lxor rget core k);
        incr core
    (* シフト命令 *)
    | OpSll, i, j, k ->
        rset core i (rget core j lsl k);
        incr core
    | OpSllv, i, j, k ->
        rset core i (rget core j lsl rget core k);
        incr core
    | OpSrl, i, j, k ->
        rset core i (rget core j lsr k);
        incr core
    | OpSrlv, i, j, k ->
        rset core i (rget core j lsr rget core k);
        incr core
    | OpSra, i, j, k ->
        rset core i (rget core j asr rget core k);
        incr core
    | OpSrav, i, j, k ->
        rset core i (rget core j asr k);
        incr core
    (* 制御命令 *)
    | OpSlt, i, j, k ->
        rset core i (if rget core j < rget core k then 1 else 0);
        incr core
    | OpSlti, i, j, k ->
        rset core i (if rget core j < k then 1 else 0);
        incr core
    | OpJump, i, _, _ ->
        core.pc := i
    | OpJal, i, _, _ ->
        rset core (regnum_of_string "$ra") (!(core.pc) + 8);
        core.pc := i
    | OpJr, i, _, _ ->
        core.pc := rget core i
    | OpBne, i, j, k ->
        core.pc := if rget core i <> rget core j then k else !(core.pc) + 1
    | OpBeq, i, j, k ->
        core.pc := if rget core i = rget core j then k else !(core.pc) + 1
    | OpHalt, _, _, _ -> raise ExecutionEnd
    (* メモリ命令 *)
    | OpLi, i, j, _
    | OpLui,  i, j, _ (* no assertion *) ->
        rset core i j;
        incr core
    | OpLw, i, d, from ->
        (try rset core i (IH.find core.mem (d + core.reg.(from)))
         with Not_found -> rset core i 0);
        incr core
    | OpSw, i, d, from ->
        IH.add core.mem (d + core.reg.(from)) core.reg.(i);
        incr core

let dump_memory core =
  IH.iter
    (fun k v -> Printf.printf "%d -> %d\n" k v)
    core.mem

let dump_memory_alist core = IH.fold (fun x y a -> (x, y) :: a) core.mem []

let exec_once core =
  exec_oneline core;
  core

let exec_all core =
  (try while true do
       exec_oneline core
     done with ExecutionEnd -> ());
  core

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

let file filename =
  Program.update_label (label_parse_file filename);
  Program.update_lines (parse_file filename);
  verify_operands ()
let string str =
  (* 最後に改行がないとまずいので... *)
  let str = str ^ "\n" in
  Program.update_label (label_parse str);
  Program.update_lines (parse str);
  verify_operands ()

(* filename内の1引数1返り値の関数を実行する *)
let exec_func filename funcname arg =
  file filename;
  let start = List.assoc funcname !(Program.g_label) in
  let core = empty_core () in
  core.pc := start;
  rset core 4 arg;
  ignore (exec_all core);
  core.reg.(2)

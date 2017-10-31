open Core
open Operand
open Operator
open Label
open Program

exception ExecutionEnd

(* ユーティリティ関数 *)
let incr () = !g_core.pc := !(!g_core.pc) + 1
let rget i = !g_core.reg.(i)
let rset i n = !g_core.reg.(i) <- n
let set_verified idx line =
  !(g_program_verified).(idx) <- line

(* Program.g_programのオペランドがあっているかを確認し、
   簡略化したプログラムをg_program_verifiedに格納する。 *)
let verify_operands () =
  g_lnum := Array.length !(g_program);
  g_program_verified := Array.make !(g_lnum) (OpHalt, 0,0,0);
  for index = 0 to !(g_lnum) - 1 do
    match !(g_program).(index) with
    (* 算術命令 *)
    | OpAdd,  (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpAdd,  i, j, k)
    | OpSub,  (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpSub,  i, j, k)
    | OpAddi, (Reg i, Reg j, Imm k)  -> !(g_program_verified).(index) <- (OpAddi, i, j, k)
    (* 論理命令 *)
    | OpAnd,  (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpAnd,  i, j, k)
    | OpOr,   (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpOr,   i, j, k)
    | OpNor,  (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpNor,  i, j, k)
    | OpXor,  (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpXor,  i, j, k)
    (* シフト命令 *)
    | OpSll,  (Reg i, Reg j, Imm k)  -> !(g_program_verified).(index) <- (OpSll,  i, j, k)
    | OpSllv, (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpSllv, i, j, k)
    | OpSrl,  (Reg i, Reg j, Imm k)  -> !(g_program_verified).(index) <- (OpSrl,  i, j, k)
    | OpSrlv, (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpSrlv, i, j, k)
    | OpSra,  (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpSra,  i, j, k)
    | OpSrav, (Reg i, Reg j, Imm k)  -> !(g_program_verified).(index) <- (OpSrav, i, j, k)
    (* 制御命令 *)
    | OpSlt,  (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpSlt,  i, j, k)
    | OpSlti, (Reg i, Reg j, Imm k)  -> !(g_program_verified).(index) <- (OpSlti, i, j, k)
    | OpJump, (Dest i, Empty, Empty) -> !(g_program_verified).(index) <- (OpJump, i, 0, 0)
    | OpJal,  (Dest i, Empty, Empty) -> !(g_program_verified).(index) <- (OpJal,  i, 0, 0)
    | OpJr,   (Reg i, Empty, Empty)  -> !(g_program_verified).(index) <- (OpJr,   i, 0, 0)
    | OpBne,  (Reg i, Reg j, Dest k) -> !(g_program_verified).(index) <- (OpBne,  i, j, k)
    | OpBeq,  (Reg i, Reg j, Dest k) -> !(g_program_verified).(index) <- (OpBeq,  i, j, k)
    | OpHalt, _                      -> !(g_program_verified).(index) <- (OpHalt, 0, 0, 0)
    (* メモリ命令 *)
    | OpLi,   (Reg i, Imm j, Empty)  -> !(g_program_verified).(index) <- (OpLi,   i, j, 0)
    | OpLui,  (Reg i, Imm j, Empty)  -> !(g_program_verified).(index) <- (OpLui,  i, j, 0)
    | OpLw,   (Reg i, RelReg (d, from), Empty) -> !(g_program_verified).(index) <- (OpLw, i, d, from)
    | OpSw,   (Reg i, RelReg (d, from), Empty) -> !(g_program_verified).(index) <- (OpSw, i, d, from)
    | op, _ -> failwith ("bad operand: " ^ (List.assoc op Operator.op_alist_rev))
  done

(* 命令を一行実行する *)
let exec_oneline : line_verified -> unit = function
  (* 算術命令 *)
  | OpAdd, i, j, k ->
      rset i (rget j + rget k);
      incr ()
  | OpSub, i, j, k ->
      rset i (rget j - rget k);
      incr ()
  | OpAddi, i, j, k ->
      rset i (rget j + k);
      incr ()
  (* 論理命令 *)
  | OpAnd, i, j, k ->
      rset i (rget j land rget k);
      incr ()
  | OpOr, i, j, k ->
      rset i (rget j lor rget k);
      incr ()
  | OpNor, i, j, k ->
      rset i (lnot (rget j lor rget k));
      incr ()
  | OpXor, i, j, k ->
      rset i (rget j lxor rget k);
      incr ()
  (* シフト命令 *)
  | OpSll, i, j, k ->
      rset i (rget j lsl k);
      incr ()
  | OpSllv, i, j, k ->
      rset i (rget j lsl rget k);
      incr ()
  | OpSrl, i, j, k ->
      rset i (rget j lsr k);
      incr ()
  | OpSrlv, i, j, k ->
      rset i (rget j lsr rget k);
      incr ()
  | OpSra, i, j, k ->
      rset i (rget j asr rget k);
      incr ()
  | OpSrav, i, j, k ->
      rset i (rget j asr k);
      incr ()
  (* 制御命令 *)
  | OpSlt, i, j, k ->
      rset i (if rget j < rget k then 1 else 0);
      incr ()
  | OpSlti, i, j, k ->
      rset i (if rget j < k then 1 else 0);
      incr ()
  | OpJump, i, _, _ ->
      !g_core.pc := i
  | OpJal, i, _, _ ->
      rset (regnum_of_string "$ra") (!(!g_core.pc) + 8);
      !g_core.pc := i
  | OpJr, i, _, _ ->
      !g_core.pc := rget i
  | OpBne, i, j, k ->
      !g_core.pc := if rget i <> rget j then k else !(!g_core.pc) + 1
  | OpBeq, i, j, k ->
      !g_core.pc := if rget i = rget j then k else !(!g_core.pc) + 1
  | OpHalt, _, _, _ -> raise ExecutionEnd
  (* メモリ命令 *)
  | OpLi, i, j, _
  | OpLui,  i, j, _ (* no assertion *) ->
      rset i j;
      incr ()
  | OpLw, i, d, from ->
      (try rset i !g_core.mem.(d + !g_core.reg.(from))
       with Not_found -> rset i 0);
      incr ()
  | OpSw, i, d, from ->
      !g_core.mem.((d + !g_core.reg.(from))) <- !g_core.reg.(i);
      incr ()

let dump_memory () =
  Array.iteri
    (fun k v -> Printf.printf "%d -> %d\n" k v)
    !g_core.mem

let dump_memory_alist () =
  let ans = ref [] in
  Array.iteri
    (fun k v -> ans := (k, v) :: !ans) !g_core.mem;
  !ans

let exec_once () =
  exec_oneline !g_program_verified.(!(!g_core.pc));
  !g_core

let exec_all () =
  (try while true
     do
       (* ログを更新 *)
       !g_core.count := !(!g_core.count) + 1;
       (* pcの指す行を実行する *)
       exec_oneline !g_program_verified.(!(!g_core.pc))
     done with _ -> ()); (* Intex_error or ExecutionEnd *)
  !g_core

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
  load_empty_core ();
  g_label := label_parse_file filename;
  g_program := parse_file filename;
  verify_operands ()
let string str =
  (* 最後に改行がないとまずいので... *)
  let str = str ^ "\n" in
  load_empty_core ();
  g_label := label_parse str;
  g_program := parse str;
  verify_operands ()

(* filename内の1引数1返り値の関数を実行する *)
let exec_func filename funcname arg =
  file filename;
  let start = List.assoc funcname !(g_label) in
  !g_core.pc := start;
  rset 4 arg;
  ignore (exec_all ());
  !g_core.reg.(2)

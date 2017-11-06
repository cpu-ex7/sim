open Core
open Operand
open Operator
open Label
open Program

exception ExecutionEnd

(* ユーティリティ関数 *)
let next_pc () = !(!g_core.pc) + 1
let incr () = !g_core.pc := !(!g_core.pc) + 1
let jump i = !g_core.pc := i
let rget i = !g_core.reg.(i)
let rset i n = !g_core.reg.(i) <- n
let mget i = !g_core.mem.(i)
let mset i j = !g_core.mem.(i) <- j
let rgetf i = !g_core.freg.(i)
let rsetf i n = !g_core.freg.(i) <- n
let mgetf i = !g_core.fmem.(i)
let msetf i j = !g_core.fmem.(i) <- j
let cget () = !(!g_core.cc)
let cset b = !g_core.cc := b
let round_even f =
  let d = f -. (float_of_int @@ int_of_float f) in
  if d < 0.5 then int_of_float f
  else if d > 0.5 then int_of_float @@ f +. 1.0
  else
    (* f = x.5 のとき: 偶数丸め *)
    let i = int_of_float f in
    if i mod 2 = 0 then i
    else i + 1

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
    (* float命令 *)
    | OpSwc1, (Reg i, Reg j, Imm k)  -> !(g_program_verified).(index) <- (OpSwc1, i, j, k)
    | OpLwc1, (Reg i, Reg j, Imm k)  -> !(g_program_verified).(index) <- (OpLwc1, i, j, k)
    | OpSwc2, (Reg i,     _,     _)  -> !(g_program_verified).(index) <- (OpSwc2, i, 0, 0)
    | OpLwc2, (Reg i,     _,     _)  -> !(g_program_verified).(index) <- (OpLwc2, i, 0, 0)
    | OpAddf, (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpAddf, i, j, k)
    | OpSubf, (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpSubf, i, j, k)
    | OpDivf, (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpDivf, i, j, k)
    | OpMulf, (Reg i, Reg j, Reg k)  -> !(g_program_verified).(index) <- (OpMulf, i, j, k)
    | OpSqrt, (Reg i, Reg j,     _)  -> !(g_program_verified).(index) <- (OpDivf, i, j, 0)
    | OpAbs,  (Reg i, Reg j,     _)  -> !(g_program_verified).(index) <- (OpMulf, i, j, 0)
    (* float変換命令 *)
    | OpMfc1,  (Reg i, Reg j,     _) -> !(g_program_verified).(index) <- (OpMfc1,  i, j, 0)
    | OpMfc2,  (Reg i, Reg j,     _) -> !(g_program_verified).(index) <- (OpMfc2,  i, j, 0)
    | OpRevn,  (Reg i, Reg j,     _) -> !(g_program_verified).(index) <- (OpRevn,  i, j, 0)
    | OpCvtsw, (Reg i, Reg j,     _) -> !(g_program_verified).(index) <- (OpCvtsw, i, j, 0)
    (* float比較命令 *)
    | OpEqf, (Reg i, Reg j, _)       -> !(g_program_verified).(index) <- (OpEqf, i, j, 0)
    | OpNef, (Reg i, Reg j, _)       -> !(g_program_verified).(index) <- (OpNef, i, j, 0)
    | OpLtf, (Reg i, Reg j, _)       -> !(g_program_verified).(index) <- (OpLtf, i, j, 0)
    | OpLef, (Reg i, Reg j, _)       -> !(g_program_verified).(index) <- (OpLef, i, j, 0)
    | OpGtf, (Reg i, Reg j, _)       -> !(g_program_verified).(index) <- (OpGtf, i, j, 0)
    | OpGef, (Reg i, Reg j, _)       -> !(g_program_verified).(index) <- (OpGef, i, j, 0)
    (* float制御命令 *)
    | OpBct, (Reg i, _, _)           -> !(g_program_verified).(index) <- (OpBct, i, 0, 0)
    | OpBcf, (Reg i, _, _)           -> !(g_program_verified).(index) <- (OpBcf, i, 0, 0)
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
  | OpAdd,  i, j, k -> rset i (rget j + rget k); incr ()
  | OpSub,  i, j, k -> rset i (rget j - rget k); incr ()
  | OpAddi, i, j, k -> rset i (rget j + k);      incr ()
  (* 論理命令 *)
  | OpAnd, i, j, k -> rset i (rget j land rget k);       incr ()
  | OpOr,  i, j, k -> rset i (rget j lor rget k);        incr ()
  | OpNor, i, j, k -> rset i (lnot (rget j lor rget k)); incr ()
  | OpXor, i, j, k -> rset i (rget j lxor rget k);       incr ()
  (* シフト命令 *)
  | OpSll,  i, j, k -> rset i (rget j lsl k);      incr ()
  | OpSllv, i, j, k -> rset i (rget j lsl rget k); incr ()
  | OpSrl,  i, j, k -> rset i (rget j lsr k);      incr ()
  | OpSrlv, i, j, k -> rset i (rget j lsr rget k); incr ()
  | OpSra,  i, j, k -> rset i (rget j asr rget k); incr ()
  | OpSrav, i, j, k -> rset i (rget j asr k);      incr ()
  (* 制御命令 *)
  | OpSlt,  i, j, k -> rset i (if rget j < rget k then 1 else 0); incr ()
  | OpSlti, i, j, k -> rset i (if rget j < k      then 1 else 0); incr ()
  | OpJump, i, _, _ -> jump i
  | OpJr,   i, _, _ -> jump @@ rget i
  | OpJal,  i, _, _ -> rset (regnum_of_string "$ra") (!(!g_core.pc) + 1); jump i
  | OpBne,  i, j, k -> jump @@ if rget i <> rget j then k else !(!g_core.pc) + 1
  | OpBeq,  i, j, k -> jump @@ if rget i = rget j  then k else !(!g_core.pc) + 1
  | OpHalt, _, _, _ -> raise ExecutionEnd
  (* float命令 *)
  | OpLwc1, i, j, k -> rsetf i !g_core.fmem.(k + rget j);        incr ()
  | OpSwc1, i, j, k -> msetf (k + rget j) (rgetf i);             incr ()
  | OpLwc2, i, _, _ -> rsetf i (float_of_string (read_line ())); incr ()
  | OpSwc2, i, _, _ -> print_float @@ rgetf i;                   incr ()
  | OpAddf, i, j, k -> rsetf i (rgetf j +. rgetf k);             incr ()
  | OpSubf, i, j, k -> rsetf i (rgetf j -. rgetf k);             incr ()
  | OpMulf, i, j, k -> rsetf i (rgetf j *. rgetf k);             incr ()
  | OpDivf, i, j, k -> rsetf i (rgetf j /. rgetf k);             incr ()
  | OpSqrt, i, j, _ -> rsetf i (sqrt @@ rgetf j);                incr ()
  | OpAbs,  i, j, _ -> rsetf i (abs_float @@ rgetf j);           incr ()
  (* float変換命令 *)
  | OpMfc1, i, j, _  -> rset  i @@ Int32.to_int @@ Int32.bits_of_float @@ rgetf j; incr ()
  | OpMfc2, i, j, _  -> rsetf i @@ Int32.float_of_bits @@ Int32.of_int @@ rget  j; incr ()
  | OpRevn, i, j, _ -> rset i @@ round_even @@ rgetf j;                            incr ()
  | OpCvtsw, i, j, _ -> rsetf i @@ float_of_int @@ rget j;                         incr ()
  (* float比較命令 *)
  | OpEqf, i, j, _ -> cset (rgetf i = rgetf j);  incr ()
  | OpNef, i, j, _ -> cset (rgetf i <> rgetf j); incr ()
  | OpLtf, i, j, _ -> cset (rgetf i < rgetf j);  incr ()
  | OpLef, i, j, _ -> cset (rgetf i <= rgetf j); incr ()
  | OpGtf, i, j, _ -> cset (rgetf i > rgetf j);  incr ()
  | OpGef, i, j, _ -> cset (rgetf i >= rgetf j); incr ()
  (* float制御命令 *)
  | OpBct, i, _, _ -> jump @@ if      cget ()  then i else next_pc ()
  | OpBcf, i, _, _ -> jump @@ if not (cget ()) then i else next_pc ()
  (* メモリ命令 *)
  | OpLi,   i, j, _
  | OpLui,  i, j, _ -> rset i j;                    incr ()
  | OpLw,   i, j, k -> rset i @@ mget (j + rget k); incr ()
  | OpSw,   i, j, k -> mset (j + rget k) (rget i);  incr ()

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

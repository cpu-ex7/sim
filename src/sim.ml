open Core
open Operand
open Operator
open Label
open Program

exception ExecutionEnd

(* ユーティリティ関数 *)
let next_pc core = !(core.pc) + 1
let incr core = core.pc := !(core.pc) + 1
let jump core i = core.pc := i
let rget core i = core.reg.(i)
let rset core i n = core.reg.(i) <- n
let mget core i = core.mem.(i)
let mset core i j = core.mem.(i) <- j
let rgetf core i = core.freg.(i)
let rsetf core i n = core.freg.(i) <- n
let mgetf core i = core.fmem.(i)
let msetf core i j = core.fmem.(i) <- j
let cget core  = !(core.cc)
let cset core b = core.cc := b
let round_even f =
  let round_even_i f =
    let d = f -. (float_of_int @@ int_of_float f) in
    if d < 0.5 then int_of_float f
    else if d > 0.5 then int_of_float @@ f +. 1.0
    else
      (* f = x.5 のとき: 偶数丸め *)
      let i = int_of_float f in
      if i mod 2 = 0 then i
      else i + 1 in
  float_of_int @@ round_even_i f

(* 命令を一行実行する *)
let execute core = function
  (* 算術命令 *)
  | OpAdd,  i, j, k -> rset core i (rget core j + rget core k); incr core
  | OpSub,  i, j, k -> rset core i (rget core j - rget core k); incr core
  | OpAddi, i, j, k -> rset core i (rget core j + k);      incr core
  (* 論理命令 *)
  | OpAnd, i, j, k -> rset core i (rget core j land rget core k);       incr core
  | OpOr,  i, j, k -> rset core i (rget core j lor rget core k);        incr core
  | OpNor, i, j, k -> rset core i (lnot (rget core j lor rget core k)); incr core
  | OpXor, i, j, k -> rset core i (rget core j lxor rget core k);       incr core
  (* シフト命令 *)
  | OpSll,  i, j, k -> rset core i (rget core j lsl k);           incr core
  | OpSllv, i, j, k -> rset core i (rget core j lsl rget core k); incr core
  | OpSrl,  i, j, k -> rset core i (rget core j lsr k);           incr core
  | OpSrlv, i, j, k -> rset core i (rget core j lsr rget core k); incr core
  | OpSra,  i, j, k -> rset core i (rget core j asr rget core k); incr core
  | OpSrav, i, j, k -> rset core i (rget core j asr k);           incr core
  (* 制御命令 *)
  | OpSlt,  i, j, k -> rset core i (if rget core j < rget core k then 1 else 0); incr core
  | OpSlti, i, j, k -> rset core i (if rget core j < k      then 1 else 0);      incr core
  | OpJump, i, _, _ -> jump core i
  | OpJr,   i, _, _ -> jump core @@ rget core i
  | OpJal,  i, _, _ -> rset core (regnum_of_string "$ra") (!(core.pc) + 1); jump core i
  | OpBne,  i, j, k -> jump core @@ if rget core i <> rget core j then k else !(core.pc) + 1
  | OpBeq,  i, j, k -> jump core @@ if rget core i = rget core j  then k else !(core.pc) + 1
  | OpHalt, _, _, _ -> raise ExecutionEnd
  (* float命令 *)
  | OpLwc1, i, j, k -> rsetf core i core.fmem.(k + rget core j);      incr core
  | OpSwc1, i, j, k -> msetf core (k + rget core j) (rgetf core i);   incr core
  | OpLwc2, i, _, _ -> rsetf core i (float_of_string (read_line ())); incr core
  | OpSwc2, i, _, _ -> print_float @@ rgetf core i;                   incr core
  | OpAddf, i, j, k -> rsetf core i (rgetf core j +. rgetf core k);   incr core
  | OpSubf, i, j, k -> rsetf core i (rgetf core j -. rgetf core k);   incr core
  | OpMulf, i, j, k -> rsetf core i (rgetf core j *. rgetf core k);   incr core
  | OpDivf, i, j, k -> rsetf core i (rgetf core j /. rgetf core k);   incr core
  | OpSqrt, i, j, _ -> rsetf core i (sqrt @@ rgetf core j);           incr core
  | OpAbs,  i, j, _ -> rsetf core i (abs_float @@ rgetf core j);      incr core
  (* float変換命令 *)
  | OpMfc1, i, j, _  -> rset core  i @@ Int32.to_int @@ Int32.bits_of_float @@ rgetf core j; incr core
  | OpMfc2, i, j, _  -> rsetf core i @@ Int32.float_of_bits @@ Int32.of_int @@ rget core  j; incr core
  | OpRevn, i, j, _ -> rsetf core i @@ round_even @@ rgetf core j;                           incr core
  | OpCvtsw, i, j, _ -> rsetf core i @@ float_of_int @@ rget core j;                         incr core
  (* float比較命令 *)
  | OpEqf, i, j, _ -> cset core (rgetf core i = rgetf core j);  incr core
  | OpNef, i, j, _ -> cset core (rgetf core i <> rgetf core j); incr core
  | OpLtf, i, j, _ -> cset core (rgetf core i < rgetf core j);  incr core
  | OpLef, i, j, _ -> cset core (rgetf core i <= rgetf core j); incr core
  | OpGtf, i, j, _ -> cset core (rgetf core i > rgetf core j);  incr core
  | OpGef, i, j, _ -> cset core (rgetf core i >= rgetf core j); incr core
  (* float制御命令 *)
  | OpBct, i, _, _ -> jump core @@ if      cget core then i else next_pc core
  | OpBcf, i, _, _ -> jump core @@ if not (cget core) then i else next_pc core
  (* メモリ命令 *)
  | OpLi,   i, j, _
  | OpLui,  i, j, _ -> rset core i j;                    incr core
  | OpLw,   i, j, k -> rset core i @@ mget core (j + rget core k); incr core
  | OpSw,   i, j, k -> mset core (j + rget core k) (rget core i);  incr core

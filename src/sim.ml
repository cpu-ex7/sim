open Int32
open Core
open Operand
open Operator
open Label
open Program

exception ExecutionEnd

(* ユーティリティ関数 *)
let next_pc core = add !(core.pc) one
let incr core = core.pc := add !(core.pc) one
let jump core i = core.pc := i
let rget core i = core.reg.(to_int i)
let rset core i n = core.reg.(to_int i) <- n
let mget core i = core.mem.(to_int i)
let mgetf core i = float_of_bits core.mem.(to_int i)
let mset core i j = core.mem.(to_int i) <- j
let msetf core i j = core.mem.(to_int i) <- bits_of_float j
let rgetf core i = float_of_bits core.freg.(to_int i) (*fregがint32*)
let rsetf core i n = core.freg.(to_int i) <- bits_of_float n (*fregがint32*)
let cget core  = !(core.cc)
let cset core b = core.cc := b
let get_input core =
  core.input_index := !(core.input_index) + 1;
  !(core.input_string).[!(core.input_index) - 1] |> Char.code |> of_int
(*
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
*)

let round_even f =
  let d = f -. (float_of_int @@ int_of_float f) in
    if(d < 0.5) then Int32.float_of_bits @@ Int32.of_float f
    else Int32.float_of_bits @@ Int32.succ @@ Int32.of_float f

let shift_left a b =
  shift_left a (to_int b)
let shift_right a b =
  shift_right a (to_int b)
let shift_right_logical a b =
  shift_right_logical a (to_int b)

(* 1 << 16 - 1 *)
(* luiで使用 *)
let shift_num = of_int 65535

(* 命令を一行実行する *)
let execute core = function
  (* 算術命令 *)
  | OpAdd,  i, j, k -> rset core i (add (rget core j) (rget core k)); incr core
  | OpSub,  i, j, k -> rset core i (sub (rget core j) (rget core k)); incr core
  | OpAddi, i, j, k -> rset core i (add (rget core j)             k); incr core
  (* 論理命令 *)
  | OpAnd, i, j, k -> rset core i (logand (rget core j) (rget core k)); incr core
  | OpOr,  i, j, k -> rset core i (logor  (rget core j) (rget core k)); incr core
  | OpOri, i, j, k -> rset core i (logor  (rget core j)             k); incr core
  | OpNor, i, j, k -> rset core i (lognot (logor (rget core j) (rget core k))); incr core
  | OpXor, i, j, k -> rset core i (logxor (rget core j) (rget core k)); incr core
  (* シフト命令 *)
  | OpSll,  i, j, k -> rset core i (shift_left          (rget core j)             k); incr core
  | OpSllv, i, j, k -> rset core i (shift_left          (rget core j) (rget core k)); incr core
  | OpSrl,  i, j, k -> rset core i (shift_right_logical (rget core j)             k); incr core
  | OpSrlv, i, j, k -> rset core i (shift_right_logical (rget core j) (rget core k)); incr core
  | OpSra,  i, j, k -> rset core i (shift_right         (rget core j) (rget core k)); incr core
  | OpSrav, i, j, k -> rset core i (shift_right         (rget core j)             k); incr core
  (* 制御命令 *)
  | OpSlt,  i, j, k -> rset core i (if rget core j < rget core k then one else zero); incr core
  | OpSlti, i, j, k -> rset core i (if rget core j < k      then one else zero);      incr core
  | OpJump, i, _, _ -> jump core i
  | OpJr,   i, _, _ -> jump core @@ rget core i
  | OpJal,  i, _, _ -> rset core (of_int 31) (next_pc core); jump core i
  | OpBne,  i, j, k -> jump core @@ if rget core i <> rget core j then k else next_pc core
  | OpBeq,  i, j, k -> jump core @@ if rget core i = rget core j  then k else next_pc core
  | OpHalt, _, _, _ -> raise ExecutionEnd
  (* float命令 *)
  | OpLwc1, i, j, k -> rsetf core i (mgetf core (add j (rget core k))); incr core
  | OpSwc1, i, j, k -> msetf core (add j (rget core k)) (rgetf core i); incr core
  | OpLwc2, i, _, _ -> rset core i (get_input core);                    incr core
  | OpSwc2, i, _, _ -> print_char @@ Char.chr @@ to_int @@ rget core i; incr core
  | OpAddf, i, j, k -> rsetf core i (rgetf core j +. rgetf core k);     incr core
  | OpSubf, i, j, k -> rsetf core i (rgetf core j -. rgetf core k);     incr core
  | OpMulf, i, j, k -> rsetf core i (rgetf core j *. rgetf core k);     incr core
  | OpDivf, i, j, k -> rsetf core i (rgetf core j /. rgetf core k);     incr core
  | OpSqrt, i, j, _ -> rsetf core i (sqrt @@ rgetf core j);             incr core
  | OpAbs,  i, j, _ -> rsetf core i (abs_float @@ rgetf core j);        incr core
  | OpMvf,  i, j, _ -> rsetf core i (rgetf core j);                     incr core
  (* float変換命令 *)
  | OpMfc1, i, j, _  -> rset core  i @@ Int32.bits_of_float @@ rgetf core j; incr core
  | OpMfc2, i, j, _  -> rsetf core i @@ Int32.float_of_bits @@ rget core  j; incr core
  | OpRevn, i, j, _  -> rsetf core i @@ round_even @@ rgetf core j;          incr core
  | OpCvtsw, i, j, _ -> rsetf core i @@ Int32.to_float @@ Int32.bits_of_float @@ rgetf core j;        incr core
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
  | OpLui,  i, j, _ -> rset core i (Int32.shift_left j 16); incr core
  | OpLw,   i, j, k -> rset core i @@ mget core (add j @@ rget core k); incr core
  | OpSw,   i, j, k -> mset core (add j @@ rget core k) (rget core i);  incr core

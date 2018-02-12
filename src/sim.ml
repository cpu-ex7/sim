open Int32
open Core
open Operand
open Operator
open Label
open Program
open Float32

exception ExecutionEnd

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
  | OpAdd, i, j, k ->
      let value = add (rget core j) (rget core k) in
      rset core i value;
      incr core
  | OpSub, i, j, k ->
      let value = sub (rget core j) (rget core k) in
      rset core i value;
      incr core
  | OpAddi, i, j, k ->
      let value = add (rget core j) k in
      rset core i value;
      incr core

  (* 論理命令 *)
  | OpAnd, i, j, k ->
      let value = logand (rget core j) (rget core k) in
      rset core i value;
      incr core
  | OpOr, i, j, k ->
      let value = logor (rget core j) (rget core k) in
      rset core i value;
      incr core
  | OpOri, i, j, k ->
      let value = logor (rget core j) k in
      rset core i value;
      incr core
  | OpNor, i, j, k ->
      let value = lognot (logor (rget core j) (rget core k)) in
      rset core i value;
      incr core
  | OpXor, i, j, k ->
      let value = logxor (rget core j) (rget core k) in
      rset core i value;
      incr core

  (* シフト命令 *)
  | OpSll, i, j, k ->
      let value = shift_left (rget core j) k in
      rset core i value;
      incr core
  | OpSllv, i, j, k ->
      let value = shift_left (rget core j) (rget core k) in
      rset core i value;
      incr core
  | OpSrl, i, j, k ->
      let value = shift_right_logical (rget core j) k in
      rset core i value;
      incr core
  | OpSrlv, i, j, k ->
      let value = shift_right_logical (rget core j) (rget core k) in
      rset core i value;
      incr core
  | OpSra, i, j, k ->
      let value = shift_right (rget core j) (rget core k) in
      rset core i value;
      incr core
  | OpSrav, i, j, k ->
      let value = shift_right (rget core j) k in
      rset core i value;
      incr core

  (* 制御命令 *)
  | OpSlt, i, j, k ->
      let value = if rget core j < rget core k then one else zero in
      rset core i value;
      incr core
  | OpSlti, i, j, k ->
      let value = if rget core j < k then one else zero in
      rset core i value;
      incr core
  | OpJump, i, _, _ ->
      let dest = add (pc core) i in
      jump core dest
  | OpJr, i, _, _ ->
      let dest = rget core i in
      jump core dest
  | OpJal, i, _, _ ->
      let dest = add (pc core) i in
      rset core regnum_lr (next_pc core);
      jump core dest
  | OpJalr, i, _, _ ->
      let dest = rget core i in
      rset core regnum_lr (next_pc core);
      jump core dest

  | OpBne, i, j, k ->
      let dest =
        if rget core i <> rget core j
        then add k (pc core)
        else next_pc core in
      jump core dest
  | OpBeq, i, j, k ->
      let dest =
        if rget core i = rget core j
        then add k (pc core)
        else next_pc core in
      jump core dest
  (* float命令 *)
  | OpSwc1, i, j, k ->
      let dest = add j (rget core k) in
      let value = rgetf core i in
      mset core dest value;
      incr core
  | OpSwc2, i, _, _ ->
      let c = Char.chr (to_int (rget core i)) in
      print_char c;
      flush_all ();
      incr core
  | OpLwc1, i, j, k ->
      let value = mget core (addf32 j (rget core k)) in
      rsetf core i value;
      incr core
  | OpLwc2, i, _, _ ->
      (* 評価はリストの最後から行われることに注意 *)
      let inputs = [get_input core;
                    get_input core;
                    get_input core;
                    get_input core;] in
      let num =
        List.fold_left (fun acc elem -> (logor (shift_left acc 7l) elem))
          0l
          inputs in
      rset core i num;
      incr core
  | OpAddf, i, j, k ->
      let value = addf32 (rgetf core j) (rgetf core k) in
      rsetf core i value;
      incr core
  | OpSubf, i, j, k ->
      let value = subf32 (rgetf core j) (rgetf core k) in
      rsetf core i value;
      incr core
  | OpMulf, i, j, k ->
      let value = mulf32 (rgetf core j) (rgetf core k) in
      rsetf core i value;
      incr core
  | OpDivf, i, j, k ->
      let value = divf32 (rgetf core j) (rgetf core k) in
      rsetf core i value;
      incr core
  | OpSqrt, i, j, _ ->
      rsetf core i (sqrt32 @@ rgetf core j);
      incr core
  | OpAbs, i, j, _ ->
      let value = abs_float32 (rgetf core j) in
      rsetf core i value;
      incr core
  | OpMvf, i, j, _ ->
      rsetf core i (rgetf core j);
      incr core

  (* float変換命令 *)
  | OpMfc1, i, j, _ ->
      let value = rgetf core j in
      rset core i value;
      incr core
  | OpMfc2, i, j, _ ->
      let value = rget core j in
      rsetf core i value;
      incr core;
  | OpRevn, i, j, _ ->
      let value = round_even32 (rgetf core j) in
      rsetf core i value;
      incr core
  | OpCvtsw, i, j, _ ->
      let value = float32_of_int (rgetf core j) in
      rsetf core i value;
      incr core

  (* float比較命令 *)
  | OpEqf, i, j, _ ->
      let cond = rgetf core i = rgetf core j in
      cset core cond;
      incr core
  | OpNef, i, j, _ ->
      let cond = rgetf core i <> rgetf core j in
      cset core cond;
      incr core
  | OpLtf, i, j, _ ->
      let cond = rgetf core i < rgetf core j in
      cset core cond;
      incr core
  | OpLef, i, j, _ ->
      let cond = rgetf core i <= rgetf core j in
      cset core cond;
      incr core
  | OpGtf, i, j, _ ->
      let cond = rgetf core i > rgetf core j in
      cset core cond;
      incr core
  | OpGef, i, j, _ ->
      let cond = rgetf core i >= rgetf core j in
      cset core cond;
      incr core

  (* float制御命令 *)
  | OpBct, i, _, _ ->
      let cond = cget core in
      let dest = if cond then add (pc core) i else next_pc core in
      jump core dest
  | OpBcf, i, _, _ ->
      let cond = not (cget core) in
      let dest = if cond then add (pc core) i else next_pc core in
      jump core dest

  (* メモリ命令 *)
  | OpLui, i, j, _ ->
      let value = Int32.shift_left j 16 in
      rset core i value;
      incr core
  | OpLw, i, j, k ->
      let value = mget core (add j @@ rget core k) in
      rset core i value;
      incr core
  | OpSw, i, j, k ->
      let location = add j (rget core k) in
      let value = (rget core i) in
      mset core location value;
      incr core

  (* 特殊命令 *)
  | OpHalt, _, _, _ ->
      raise ExecutionEnd

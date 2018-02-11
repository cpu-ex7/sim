open Int32
open Operator
open Operand

type line_verified = Operator.t * int32 * int32 * int32
type t = line_verified array

(* Program.g_programのオペランドがあっているかを確認し、
   簡略化したプログラムをg_program_verifiedに格納する。 *)
let f parsed_program =
  let length = (Array.length parsed_program) in
  let verified = ref (Array.make length (OpHalt, zero,zero,zero)) in
  let update index op i j k =
    !(verified).(index) <- (op, i, j, k) in
  for index = 0 to length - 1 do
    match parsed_program.(index) with
    (* 算術命令 *)
    | OpAdd, (Reg i, Reg j, Reg k) ->
        update index OpAdd i j k
    | OpSub, (Reg i, Reg j, Reg k) ->
        update index OpSub i j k
    | OpAddi, (Reg i, Reg j, Imm k) ->
        update index OpAddi i j k
    | OpAddi, (Reg i, Reg j, Rabel k) ->
        update index OpAddi i j (ParserArgs.find_label k)

    (* 論理命令 *)
    | OpAnd, (Reg i, Reg j, Reg k) ->
        update index OpAnd i j k
    | OpOr, (Reg i, Reg j, Reg k) ->
        update index OpOr i j k
    | OpOri, (Reg i, Reg j, Imm k) ->
        update index OpOri i j k
    | OpNor, (Reg i, Reg j, Reg k) ->
        update index OpNor i j k
    | OpXor, (Reg i, Reg j, Reg k) ->
        update index OpXor i j k

    (* シフト命令 *)
    | OpSll, (Reg i, Reg j, Imm k) ->
        update index OpSll i j k
    | OpSllv, (Reg i, Reg j, Reg k) ->
        update index OpSllv i j k
    | OpSrl, (Reg i, Reg j, Imm k) ->
        update index OpSrl i j k
    | OpSrlv, (Reg i, Reg j, Reg k) ->
        update index OpSrlv i j k
    | OpSra, (Reg i, Reg j, Reg k) ->
        update index OpSra i j k
    | OpSrav, (Reg i, Reg j, Imm k) ->
        update index OpSrav i j k

    (* 制御命令 *)
    | OpSlt, (Reg i, Reg j, Reg k) ->
        update index OpSlt i j k
    | OpSlti, (Reg i, Reg j, Imm k) ->
        update index OpSlti i j k
    | OpJump, (Imm i, Empty, Empty) ->
        update index OpJump i zero zero
    | OpJump, (Rabel i, Empty, Empty) ->
        let n = Int32.sub (ParserArgs.find_label i) (of_int index) in
        update index OpJump n zero zero
    | OpJal, (Imm i, Empty, Empty) ->
        update index OpJal i zero zero
    | OpJal, (Rabel i, Empty, Empty) ->
        let n = Int32.sub (ParserArgs.find_label i) (of_int index) in
        update index OpJal n zero zero
    | OpJalr, (Reg i, Empty, Empty) ->
        update index OpJalr i zero zero
    | OpJalr, (Rabel i, Empty, Empty) ->
        let n = Int32.sub (ParserArgs.find_label i) (of_int index) in
        update index OpJal n zero zero
    | OpJr, (Reg i, Empty, Empty) ->
        update index OpJr i zero zero
    | OpJr, (Rabel i, Empty, Empty) ->
        let n = Int32.sub (ParserArgs.find_label i) (of_int index) in
        update index OpJr n zero zero
    | OpBne, (Reg i, Reg j, Imm k) ->
        update index OpBne i j k
    | OpBne, (Reg i, Reg j, Rabel k) ->
        let n = Int32.sub (ParserArgs.find_label k) (of_int index) in
        update index OpBne i j n
    | OpBeq, (Reg i, Reg j, Imm k) ->
        update index OpBeq i j k
    | OpBeq, (Reg i, Reg j, Rabel k) ->
        let n = Int32.sub (ParserArgs.find_label k) (of_int index) in
        update index OpBeq i j n
    | OpHalt, _ ->
        update index OpHalt zero zero zero

    (* float命令 *)
    | OpSwc1, (FReg i, RelReg (j, k), _) ->
        update index OpSwc1 i j k
    | OpLwc1, (FReg i, RelReg (j, k), _) ->
        update index OpLwc1 i j k
    | OpSwc2, (Reg i, _, _) ->
        update index OpSwc2 i zero zero
    | OpLwc2, (Reg i, _, _) ->
        update index OpLwc2 i zero zero
    | OpLwc3, (Reg i, _, _) ->
        update index OpLwc3 i zero zero
    | OpAddf, (FReg i, FReg j, FReg k) ->
        update index OpAddf i j k
    | OpSubf, (FReg i, FReg j, FReg k) ->
        update index OpSubf i j k
    | OpDivf, (FReg i, FReg j, FReg k) ->
        update index OpDivf i j k
    | OpMulf, (FReg i, FReg j, FReg k) ->
        update index OpMulf i j k
    | OpSqrt, (FReg i, FReg j, _) ->
        update index OpSqrt i j zero
    | OpAbs, (FReg i, FReg j, _) ->
        update index OpAbs i j zero
    | OpMvf, (FReg i, FReg j, _) ->
        update index OpMvf i j zero

    (* float変換命令 *)
    | OpMfc1, (Reg i, FReg j, _) ->
        update index OpMfc1 i j zero
    | OpMfc2, (FReg i, Reg j, _) ->
        update index OpMfc2 i j zero
    | OpRevn, (FReg i, FReg j, _) ->
        update index OpRevn i j zero
    | OpCvtsw, (FReg i, FReg j, _) ->
        update index OpCvtsw i j zero

    (* float比較命令 *)
    | OpEqf, (FReg i, FReg j, _) ->
        update index OpEqf i j zero
    | OpNef, (FReg i, FReg j, _) ->
        update index OpNef i j zero
    | OpLtf, (FReg i, FReg j, _) ->
        update index OpLtf i j zero
    | OpLef, (FReg i, FReg j, _) ->
        update index OpLef i j zero
    | OpGtf, (FReg i, FReg j, _) ->
        update index OpGtf i j zero
    | OpGef, (FReg i, FReg j, _) ->
        update index OpGef i j zero

    (* float制御命令 *)
    | OpBct, (Imm i, _, _) ->
        update index OpBct i zero zero
    | OpBcf, (Imm i, _, _) ->
        update index OpBcf i zero zero

    (* メモリ命令 *)
    | OpLui, (Reg i, Imm j, Empty) ->
        update index OpLui i j zero
    | OpLw, (Reg i, RelReg (d, from), Empty) ->
        update index OpLw i d from
    | OpSw, (Reg i, RelReg (d, from), Empty) ->
        update index OpSw i d from
    | op, _ ->
        failwith ("bad operands: " ^ (List.assoc op Operator.op_alist_rev))
  done;
  verified

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
  for index = 0 to length - 1 do
    match parsed_program.(index) with
    (* 算術命令 *)
    | OpAdd,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpAdd,  i, j, k)
    | OpSub,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpSub,  i, j, k)
    | OpAddi, (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpAddi, i, j, k)
    (* 論理命令 *)
    | OpAnd,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpAnd,  i, j, k)
    | OpOr,   (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpOr,   i, j, k)
    | OpOri,  (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpOri,  i, j, k)
    | OpNor,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpNor,  i, j, k)
    | OpXor,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpXor,  i, j, k)
    (* シフト命令 *)
    | OpSll,  (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpSll,  i, j, k)
    | OpSllv, (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpSllv, i, j, k)
    | OpSrl,  (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpSrl,  i, j, k)
    | OpSrlv, (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpSrlv, i, j, k)
    | OpSra,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpSra,  i, j, k)
    | OpSrav, (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpSrav, i, j, k)
    (* 制御命令 *)
    | OpSlt,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpSlt,  i, j, k)
    | OpSlti, (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpSlti, i, j, k)
    | OpJump, (Imm i, Empty, Empty) -> !(verified).(index) <- (OpJump, i, zero, zero)
    | OpJal,  (Imm i, Empty, Empty) -> !(verified).(index) <- (OpJal,  i, zero, zero)
    | OpJalr,  (Reg i, Empty, Empty) -> !(verified).(index) <- (OpJalr,  i, zero, zero)
    | OpJr,   (Reg i, Empty, Empty)  -> !(verified).(index) <- (OpJr,   i, zero, zero)
    | OpBne,  (Reg i, Reg j, Imm k) -> !(verified).(index) <- (OpBne,  i, j, k)
    | OpBeq,  (Reg i, Reg j, Imm k) -> !(verified).(index) <- (OpBeq,  i, j, k)
    | OpHalt, _                      -> !(verified).(index) <- (OpHalt, zero, zero, zero)
    (* float命令 *)
    | OpSwc1, (FReg i, RelReg (j, k), _) -> !(verified).(index) <- (OpSwc1, i, j, k)
    | OpLwc1, (FReg i, RelReg (j, k), _) -> !(verified).(index) <- (OpLwc1, i, j, k)
    | OpSwc2, (Reg i,     _,     _)      -> !(verified).(index) <- (OpSwc2, i, zero, zero)
    | OpLwc2, (Reg i,     _,     _)      -> !(verified).(index) <- (OpLwc2, i, zero, zero)
    | OpLwc3, (Reg i,     _,     _)      -> !(verified).(index) <- (OpLwc3, i, zero, zero)
    | OpAddf, (FReg i, FReg j, FReg k)   -> !(verified).(index) <- (OpAddf, i, j, k)
    | OpSubf, (FReg i, FReg j, FReg k)   -> !(verified).(index) <- (OpSubf, i, j, k)
    | OpDivf, (FReg i, FReg j, FReg k)   -> !(verified).(index) <- (OpDivf, i, j, k)
    | OpMulf, (FReg i, FReg j, FReg k)   -> !(verified).(index) <- (OpMulf, i, j, k)
    | OpSqrt, (FReg i, FReg j,     _)    -> !(verified).(index) <- (OpSqrt, i, j, zero)
    | OpAbs,  (FReg i, FReg j,     _)    -> !(verified).(index) <- (OpAbs, i, j, zero)
    | OpMvf,  (FReg i, FReg j,     _)    -> !(verified).(index) <- (OpMvf, i, j, zero)
    (* float変換命令 *)
    | OpMfc1,  (Reg i, FReg j,  _) -> !(verified).(index) <- (OpMfc1,  i, j, zero)
    | OpMfc2,  (FReg i, Reg j,  _) -> !(verified).(index) <- (OpMfc2,  i, j, zero)
    | OpRevn,  (FReg i, FReg j, _) -> !(verified).(index) <- (OpRevn,  i, j, zero)
    | OpCvtsw, (FReg i, FReg j, _) -> !(verified).(index) <- (OpCvtsw, i, j, zero)
    (* float比較命令 *)
    | OpEqf, (FReg i, FReg j, _) -> !(verified).(index) <- (OpEqf, i, j, zero)
    | OpNef, (FReg i, FReg j, _) -> !(verified).(index) <- (OpNef, i, j, zero)
    | OpLtf, (FReg i, FReg j, _) -> !(verified).(index) <- (OpLtf, i, j, zero)
    | OpLef, (FReg i, FReg j, _) -> !(verified).(index) <- (OpLef, i, j, zero)
    | OpGtf, (FReg i, FReg j, _) -> !(verified).(index) <- (OpGtf, i, j, zero)
    | OpGef, (FReg i, FReg j, _) -> !(verified).(index) <- (OpGef, i, j, zero)
    (* float制御命令 *)
    | OpBct, (Imm i, _, _)           -> !(verified).(index) <- (OpBct, i, zero, zero)
    | OpBcf, (Imm i, _, _)           -> !(verified).(index) <- (OpBcf, i, zero, zero)
    (* メモリ命令 *)
    | OpLui,  (Reg i, Imm j, Empty)  -> !(verified).(index) <- (OpLui,  i, j, zero)
    | OpLw,   (Reg i, RelReg (d, from), Empty) -> !(verified).(index) <- (OpLw, i, d, from)
    | OpSw,   (Reg i, RelReg (d, from), Empty) -> !(verified).(index) <- (OpSw, i, d, from)
    | op, _ -> failwith ("bad operands: " ^ (List.assoc op Operator.op_alist_rev))
  done;
  verified

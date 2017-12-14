open Operator
open Operand

type line_verified = Operator.t * int * int * int
type t = line_verified array

(* Program.g_programのオペランドがあっているかを確認し、
   簡略化したプログラムをg_program_verifiedに格納する。 *)
let f parsed_program =
  let length = (Array.length parsed_program) in
  let verified = ref (Array.make length (OpHalt, 0,0,0)) in
  for index = 0 to length - 1 do
    match parsed_program.(index) with
    (* 算術命令 *)
    | OpAdd,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpAdd,  i, j, k)
    | OpSub,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpSub,  i, j, k)
    | OpAddi, (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpAddi, i, j, k)
    (* 論理命令 *)
    | OpAnd,  (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpAnd,  i, j, k)
    | OpOr,   (Reg i, Reg j, Reg k)  -> !(verified).(index) <- (OpOr,   i, j, k)
    | OpOri,  (Reg i, Reg j, Imm k)  -> !(verified).(index) <- (OpOri, i, j, k)
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
    | OpJump, (Dest i, Empty, Empty) -> !(verified).(index) <- (OpJump, i, 0, 0)
    | OpJal,  (Dest i, Empty, Empty) -> !(verified).(index) <- (OpJal,  i, 0, 0)
    | OpJr,   (Reg i, Empty, Empty)  -> !(verified).(index) <- (OpJr,   i, 0, 0)
    | OpBne,  (Reg i, Reg j, Dest k) -> !(verified).(index) <- (OpBne,  i, j, k)
    | OpBeq,  (Reg i, Reg j, Dest k) -> !(verified).(index) <- (OpBeq,  i, j, k)
    | OpHalt, _                      -> !(verified).(index) <- (OpHalt, 0, 0, 0)
    (* float命令 *)
    | OpSwc1, (Reg i, Reg j, Imm k)      -> !(verified).(index) <- (OpSwc1, i, j, k)
    | OpLwc1, (Reg i, RelReg (j, k), _)  -> !(verified).(index) <- (OpLwc1, i, j, k)
    | OpSwc2, (Reg i,     _,     _)      -> !(verified).(index) <- (OpSwc2, i, 0, 0)
    | OpLwc2, (Reg i,     _,     _)      -> !(verified).(index) <- (OpLwc2, i, 0, 0)
    | OpAddf, (Reg i, Reg j, Reg k)      -> !(verified).(index) <- (OpAddf, i, j, k)
    | OpSubf, (Reg i, Reg j, Reg k)      -> !(verified).(index) <- (OpSubf, i, j, k)
    | OpDivf, (Reg i, Reg j, Reg k)      -> !(verified).(index) <- (OpDivf, i, j, k)
    | OpMulf, (Reg i, Reg j, Reg k)      -> !(verified).(index) <- (OpMulf, i, j, k)
    | OpSqrt, (Reg i, Reg j,     _)      -> !(verified).(index) <- (OpDivf, i, j, 0)
    | OpAbs,  (Reg i, Reg j,     _)      -> !(verified).(index) <- (OpMulf, i, j, 0)
    (* float変換命令 *)
    | OpMfc1,  (Reg i, Reg j,     _) -> !(verified).(index) <- (OpMfc1,  i, j, 0)
    | OpMfc2,  (Reg i, Reg j,     _) -> !(verified).(index) <- (OpMfc2,  i, j, 0)
    | OpRevn,  (Reg i, Reg j,     _) -> !(verified).(index) <- (OpRevn,  i, j, 0)
    | OpCvtsw, (Reg i, Reg j,     _) -> !(verified).(index) <- (OpCvtsw, i, j, 0)
    (* float比較命令 *)
    | OpEqf, (Reg i, Reg j, _)       -> !(verified).(index) <- (OpEqf, i, j, 0)
    | OpNef, (Reg i, Reg j, _)       -> !(verified).(index) <- (OpNef, i, j, 0)
    | OpLtf, (Reg i, Reg j, _)       -> !(verified).(index) <- (OpLtf, i, j, 0)
    | OpLef, (Reg i, Reg j, _)       -> !(verified).(index) <- (OpLef, i, j, 0)
    | OpGtf, (Reg i, Reg j, _)       -> !(verified).(index) <- (OpGtf, i, j, 0)
    | OpGef, (Reg i, Reg j, _)       -> !(verified).(index) <- (OpGef, i, j, 0)
    (* float制御命令 *)
    | OpBct, (Reg i, _, _)           -> !(verified).(index) <- (OpBct, i, 0, 0)
    | OpBcf, (Reg i, _, _)           -> !(verified).(index) <- (OpBcf, i, 0, 0)
    (* メモリ命令 *)
    | OpLui,  (Reg i, Imm j, Empty)  -> !(verified).(index) <- (OpLui,  i, j, 0)
    | OpLw,   (Reg i, RelReg (d, from), Empty) -> !(verified).(index) <- (OpLw, i, d, from)
    | OpSw,   (Reg i, RelReg (d, from), Empty) -> !(verified).(index) <- (OpSw, i, d, from)
    | op, _ -> failwith ("bad operands: " ^ (List.assoc op Operator.op_alist_rev))
  done;
  verified

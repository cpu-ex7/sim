type operator =
  | OpAdd
  | OpAddi
  | OpSlti
  | OpJump
  | OpHalt
  | OpBne

type operand =
  | Reg of int
  | Imm of int
  | Dest of int

type operands = operand array
type label = (string * int) list
type lines = (operator * operands) array

(* アセンブリの文字列とoperator型の値との関係 *)
let op_alist =
  [("add", OpAdd);
   ("addi", OpAddi);
   ("slti", OpSlti);
   ("j", OpJump);
   ("halt", OpHalt);
   ("bne", OpBne)]

let lookup op_str = List.assoc op_str op_alist

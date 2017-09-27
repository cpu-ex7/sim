type operator =
  | OpAdd
  | OpAddi
  | OpSlti

type operand =
  | Reg of int
  | Imm of int
type operands = operand array
type t = operator * operands

(* アセンブリの文字列とoperator型の値との関係 *)
let op_alist =
  [("add", OpAdd);
   ("addi", OpAddi);
   ("slti", OpSlti)]

let lookup op_str = List.assoc op_str op_alist

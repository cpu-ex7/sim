open List

exception Wrong_operand_numver

type reg =
  | Reg of int
  | Imm of int
type operands = reg array
type op_type = OpAdd | OpAddi
type t = op_type * operands

let op_alist =
  [("add", OpAdd);
   ("addi", OpAddi)]

let lookup op_str = List.assoc op_str op_alist

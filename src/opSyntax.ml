open List

exception Wrong_operand_numver

type reg = int
type operands = reg list
type t =
  | OpAdd of operands

let op_alist =
  [("add", (fun lst -> OpAdd lst))]

let lookup op_str operands =
  (List.assoc op_str op_alist) operands

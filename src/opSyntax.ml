type reg = int
type imm = int
type dest = int
type operator =
  (* 算術命令 *)
  | OpAdd
  | OpSub
  | OpAddi
  (* 論理命令 *)
  | OpAnd
  | OpOr
  | OpNor
  | OpXor
  (* シフト命令 *)
  | OpSll
  | OpSllv
  | OpSrl
  | OpSrlv
  | OpSra
  | OpSrav
  (* 制御命令 *)
  | OpSlt
  | OpSlti
  | OpJump
  | OpJal
  | OpJr
  | OpBne
  | OpBeq
  | OpHalt
  (* メモリ命令 *)
  | OpLi
  | OpLui
  | OpLw
  | OpSw

type operand =
  | Empty
  | Reg of reg
  | Imm of imm
  | Dest of dest
  | RelReg of (imm * reg)

type operands = operand * operand * operand
type label = (string * int) list
type lines = (operator * operands) array

(* アセンブリの文字列とoperator型の値との関係 *)
let op_alist =
  [ (* 算術命令 *)
    ("add", OpAdd);
    ("sub", OpSub);
    ("addi", OpAddi);
    (* 論理命令 *)
    ("and", OpAnd);
    ("or", OpOr);
    ("nor", OpNor);
    ("xor", OpXor);
    (* シフト命令 *)
    ("sll", OpSll);
    ("sllv", OpSllv);
    ("srl", OpSrl);
    ("srlv", OpSrlv);
    ("sra", OpSrl);
    ("srav", OpSrlv);
    (* 制御命令 *)
    ("slt", OpSlt);
    ("slti", OpSlti);
    ("j", OpJump);
    ("jal", OpJal);
    ("jr", OpJr);
    ("bne", OpBne);
    ("beq", OpBeq);
    ("halt", OpHalt);
    (* メモリ命令 *)
    ("li", OpLi);
    ("lui", OpLui);
    ("lw", OpLw);
    ("sw", OpSw);]

let op_alist_rev =
  List.map (fun (x, y) -> (y, x)) op_alist

let lookup op_str = List.assoc op_str op_alist

let string_of_regnum = function
  | 0 -> "$zero"
  | 1 -> "$at"
  | 2 -> "$v0"
  | 3 -> "$v1"
  | 4 -> "$a0"
  | 5 -> "$a1"
  | 6 -> "$a2"
  | 7 -> "$a3"
  | 8 -> "$t0"
  | 9 -> "$t1"
  | 10 -> "$t2"
  | 11 -> "$t3"
  | 12 -> "$t4"
  | 13 -> "$t5"
  | 14 -> "$t6"
  | 15 -> "$t7"
  | 16 -> "$s0"
  | 17 -> "$s1"
  | 18 -> "$s2"
  | 19 -> "$s3"
  | 20 -> "$s4"
  | 21 -> "$s5"
  | 22 -> "$s6"
  | 23 -> "$s7"
  | 24 -> "$t8"
  | 25 -> "$t9"
  | 26 -> "$k0"
  | 27 -> "$k1"
  | 28 -> "$gp"
  | 29 -> "$sp"
  | 30 -> "$fp"
  | 31 -> "$ra"
  | _ -> failwith "string_of_regnum"

let regnum_of_string = function
  | "$zero" -> 0
  | "$at" -> 1
  | "$v0" -> 2
  | "$v1" -> 3
  | "$a0" -> 4
  | "$a1" -> 5
  | "$a2" -> 6
  | "$a3" -> 7
  | "$t0" -> 8
  | "$t1" -> 9
  | "$t2" -> 10
  | "$t3" -> 11
  | "$t4" -> 12
  | "$t5" -> 13
  | "$t6" -> 14
  | "$t7" -> 15
  | "$s0" -> 16
  | "$s1" -> 17
  | "$s2" -> 18
  | "$s3" -> 19
  | "$s4" -> 20
  | "$s5" -> 21
  | "$s6" -> 22
  | "$s7" -> 23
  | "$t8" -> 24
  | "$t9" -> 25
  | "$k0" -> 26
  | "$k1" -> 27
  | "$gp" -> 28
  | "$sp" -> 29
  | "$fp" -> 30
  | "$ra" -> 31
  | _ -> failwith "regnum_of_string"

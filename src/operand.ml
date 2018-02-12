(* オペランドの型 *)
open Int32

type reg = int32
type imm = int32
type dest = int32
type t =
  | Empty
  | Reg of reg
  | FReg of reg
  | Imm of imm
  | Rabel of string
  | Dest of dest
  | RelReg of (imm * reg)

(* 数字をレジスタ文字列に変換 *)
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
  | i -> failwith @@ "invalid register number : " ^ (string_of_int i)

(* レジスタ文字列を数字に変換 *)
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
  | str -> failwith @@ "invalid register name : " ^ str

let print_operand = function
  | Empty -> ()
  | Reg i -> Printf.printf "%20s" (string_of_regnum (to_int i))
  | FReg i -> Printf.printf "%20s" ("$f" ^ (string_of_int (to_int i)))
  | Imm i -> Printf.printf "%20ld" i
  | Rabel s -> Printf.printf "%20s" s
  | Dest i -> Printf.printf "%20ld" i
  | RelReg (i, j) -> Printf.printf "%9ld(%9ld)" i j

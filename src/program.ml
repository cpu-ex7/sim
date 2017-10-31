(* プログラム *)
open Core

type operands = Operand.t * Operand.t * Operand.t
type line_verified = Operator.t * int * int * int
type t = (Operator.t * operands) array
type t_verified = line_verified array

(* グローバル変数 *)
let (g_program: t ref) = ref [||]
(* グローバル変数(オペランドの正しさを確認したもの) *)
let (g_program_verified: t_verified ref) = ref [||]
(* プログラムの行数 ( = Array.length !g_program
                      = Array.length !g_program_verified) *)
let g_lnum = ref 0

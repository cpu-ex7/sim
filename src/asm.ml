open Int32
open Printf
open Operand
open Operator

(* strの右側に"0"を入れて長さをwidthにする *)
let pad str width =
  let left = width - String.length str in
  if left <= 0 then str
  else Array.fold_left (^) "" (Array.make left "0") ^ str

(* dを2進法で表す幅widthになるまで0埋めする *)
let rec bits_of_num d width =
  let d = Int32.to_int d in
  if d < 0 then bits_of_num (of_int ((2 lsl (width - 1)) + d)) width else
  if d = 0 then pad "0" width else
    let rec loop acc d =
      if d = 0 then acc else
        loop (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    pad (String.concat "" (loop [] d)) width

(* 位置命令をビット列に変換する *)
let bits_of_line line line_num =
  match line with
  (* 算術命令 *)
  | OpAdd,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "100000"
  | OpSub,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "100010"
  | OpAddi,  i, j, k ->
      sprintf "%s%s%s%s"
        "001000" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 16)

  (* 論理命令 *)
  | OpAnd,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "100100"
  | OpOr,    i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "000000"
  | OpOri,   i, j, k ->
      sprintf "%s%s%s%s"
        "001101" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 16)
  | OpNor,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "000111"
  | OpXor,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "100110"

  (* シフト命令 *)
  | OpSll,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" "00000" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 5) "000000"
  | OpSrl,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" "00000" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 5) "000010"
  | OpSra,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" "00000" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 5) "000011"
  | OpSllv,  i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "000100"
  | OpSrlv,  i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "000110"
  | OpSrav,  i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "000111"

  (* 制御命令 *)
  | OpSlt,   i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "101010"
  | OpSlti,  i, j, k ->
      sprintf "%s%s%s%s"
        "001010" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 16)
  | OpJump,  i, _, _ ->
      sprintf "%s%s"
        "000010" (bits_of_num i 26)
  | OpJal,   i, _, _ ->
      sprintf "%s%s"
        "000010" (bits_of_num i 26)
  | OpJr,    i, _, _ ->
      sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num i 5) "00000" "00000" "00000" "001000"
  | OpBeq,   i, j, k ->
      sprintf "%s%s%s%s"
        "000100" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num (sub k @@ of_int line_num) 16)
  | OpBne,   i, j, k ->
      sprintf "%s%s%s%s"
        "000101" (bits_of_num i 5) (bits_of_num j 5) (bits_of_num (sub k @@ of_int line_num) 16)
  | OpHalt,  _, _, _  ->
      sprintf "%s%s%s%s"
        "000100" "00000" "00000" "0000000000000000"

  (* メモリ命令 *)
  | OpLui,   i, j, _ ->
      sprintf "%s%s%s%s"
        "001111" "00000" (bits_of_num i 5) (bits_of_num j 16)
  | OpLw,    i, j, k ->
      sprintf "%s%s%s%s"
        "100011" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 16)
  | OpSw,    i, j, k ->
      sprintf "%s%s%s%s"
        "101011" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 16)

  (* float命令 *)
  | OpLwc1,  i, j, k ->
      sprintf "%s%s%s%s"
        "110001" (bits_of_num k 5) (bits_of_num i 5) (bits_of_num j 16)
  | OpSwc1,  i, j, k ->
      sprintf "%s%s%s%s"
        "111001" (bits_of_num k 5) (bits_of_num i 5) (bits_of_num j 16)
  | OpLwc2,  i, _, _ ->
      sprintf "%s%s%s"
        "110010" (bits_of_num i 5) (pad "" 21)
  | OpSwc2,  i, _, _ ->
      sprintf "%s%s%s"
        "111010" (bits_of_num i 5) (pad "" 21)
  | OpAddf,  i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" (bits_of_num k 5) (bits_of_num j 5) (bits_of_num i 5) "000000"
  | OpSubf,  i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" (bits_of_num k 5) (bits_of_num j 5) (bits_of_num i 5) "000001"
  | OpMulf,  i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" (bits_of_num k 5) (bits_of_num j 5) (bits_of_num i 5) "000010"
  | OpDivf,  i, j, k ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" (bits_of_num k 5) (bits_of_num j 5) (bits_of_num i 5) "000011"
  | OpSqrt,  i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num j 5) (bits_of_num i 5) "000100"
  | OpAbs,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num j 5) (bits_of_num i 5) "000101"
  | OpMvf,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num j 5) (bits_of_num i 5) "000110"

  (* float変換 *)
  | OpMfc1,  i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" (bits_of_num i 5) (bits_of_num j 5) "00000" "001000"
  | OpMfc2,  i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" (bits_of_num j 5) (bits_of_num i 5) "00000" "001001"
  | OpRevn,  i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num j 5) (bits_of_num i 5) "001100"
  | OpCvtsw, i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num j 5) (bits_of_num i 5) "100000"

  (* float比較命令 *)
  | OpEqf,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num i 5) (bits_of_num j 5) "110101"
  | OpNef,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num i 5) (bits_of_num j 5) "111011"
  | OpLtf,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num i 5) (bits_of_num j 5) "110011"
  | OpLef,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num i 5) (bits_of_num j 5) "110111"
  | OpGtf,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num i 5) (bits_of_num j 5) "111001"
  | OpGef,   i, j, _ ->
      sprintf "%s%s%s%s%s%s"
        "010001" "00000" "00000" (bits_of_num i 5) (bits_of_num j 5) "111101"

  (* float制御命令 *)
  | OpBct,   i, _, _ ->
      sprintf "%s%s%s"
        "010001" (bits_of_num (sub i @@ of_int line_num) 20) "010001"
  | OpBcf,   i, _, _ ->
      sprintf "%s%s%s"
        "010001" (bits_of_num (sub i @@ of_int line_num) 20) "010000"

let print_assembly prog_verified =
  Array.iteri
    (fun line_num l -> print_endline (bits_of_line l line_num))
    prog_verified

let string_of_assembly prog_verified =
  let ans = ref "" in
  Array.iteri
    (fun line_num l -> ans := (!ans ^ (bits_of_line l line_num)) ^ "\n")
    prog_verified;
  !ans

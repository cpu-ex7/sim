open OpSyntax

let pad str width =
  let left = width - String.length str in
  if left <= 0 then str
  else Array.fold_left (^) "" (Array.make left "0") ^ str

let rec bits_of_num d width =
  if d < 0 then bits_of_num ((2 lsl (width - 1)) + d) width else
  if d = 0 then pad "0" width else
    let rec aux acc d =
      if d = 0 then acc else
        aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    pad (String.concat "" (aux [] d)) width

let string_of_add = function
  | [|Reg i; Reg j; Reg k|] ->
      Printf.sprintf "%s%s%s%s%s%s"
        "000000" (bits_of_num j 5) (bits_of_num k 5) (bits_of_num i 5) "00000" "100000"
  | _ -> failwith "string_of_add"

let string_of_addi = function
  | [|Reg i; Reg j; Imm k|] ->
      Printf.sprintf "%s%s%s%s"
        "001000" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 16)
  | _ -> failwith "string_of_addi"

let string_of_slti = function
  | [|Reg i; Reg j; Imm k|] ->
      Printf.sprintf "%s%s%s%s"
        "001010" (bits_of_num j 5) (bits_of_num i 5) (bits_of_num k 16)
  | _ -> failwith "string_of_slti"

let string_of_jump = function
  | [|Dest i|] ->
      Printf.sprintf "%s%s" "000010" (bits_of_num i 26)
  | _ -> failwith "string_of_slti"

let string_of_bne line_num = function
  | [|Reg i; Reg j; Dest k|] ->
      Printf.sprintf "%s%s%s%s"
        "000101" (bits_of_num i 5) (bits_of_num j 5)
        (bits_of_num (k - line_num) 16)
  | _ -> failwith "string_of_bne"

let bits_of_line (op, operands) line_num =
  match op with
  | OpAdd -> string_of_add operands
  | OpAddi -> string_of_addi operands
  | OpSlti -> string_of_slti operands
  | OpJump -> string_of_jump operands
  | OpBne -> string_of_bne line_num operands
  | OpHalt -> ""

let print_assembly () =
  Array.iteri
    (fun line_num l -> print_endline (bits_of_line l line_num))
    !Program.g_lines

let assembly_string () =
  let ans = ref "" in
  Array.iteri
    (fun line_num l -> ans := (!ans ^ (bits_of_line l line_num)) ^ "\n")
    !Program.g_lines;
  !ans

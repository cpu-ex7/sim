open OpSyntax

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)
let parse_just str = parse str |> List.hd
let parse_file filename =
  Parser.toplevel Lexer.main (Lexing.from_channel (open_in filename))

type core = {
  reg : int array
}

let empty_core () = { reg = Array.make 32 0 }

let rget core i = core.reg.(i)
let rset core i n = core.reg.(i) <- n

let exec_add core operands =
  match operands with
  | [|Reg i; Reg j; Reg k|] -> rset core i (rget core j + rget core k); core
  | _ -> failwith "add: bad operands"

let exec_addi core operands =
  match operands with
  | [|Reg i; Reg j; Imm k|] -> rset core i (rget core j + k); core
  | _ -> failwith "addi: bad operands"

let exec_slti core operands =
  match operands with
  | [|Reg i; Reg j; Imm k|] -> rset core i (rget core j - k); core
  | _ -> failwith "addi: bad operands"

let exec_oneline core line =
  match line with
  | OpAdd, operands -> exec_add core operands
  | OpAddi, operands -> exec_addi core operands
  | OpSlti, operands -> exec_slti core operands

let exec lines =
  let core = empty_core () in
  List.fold_left exec_oneline core lines

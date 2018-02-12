open Operand

type operands = Operand.t * Operand.t * Operand.t
type t = (Operator.t * operands) array

let print_program x =
  for i = 0 to (Array.length x) - 1 do
    let op, (i, j, k)  = x.(i) in
    Operator.print_operator op;
    Operand.print_operand i;
    Operand.print_operand j;
    Operand.print_operand k;
    print_newline ()
  done

let print_program_with_label x label =
  let label_inv =   List.map (fun (x, y) -> (Int32.to_int y, x)) label in
  for i = 0 to (Array.length x) - 1 do
    if try ignore (List.assoc i label_inv); true with _ -> false
    then Printf.printf "%s:\n" (List.assoc i label_inv);
    match x.(i) with
    | (op, (i, Empty, Empty)) ->
        Operator.print_operator op;
        Operand.print_operand i;
        print_newline ()
    | (op, (i, j, Empty)) ->
        Operator.print_operator op;
        Operand.print_operand i;
        print_string ",";
        Operand.print_operand j;
        print_newline ()
    | (op, (i, j, k)) ->
        Operator.print_operator op;
        Operand.print_operand i;
        print_string ",";
        Operand.print_operand j;
        print_string ",";
        Operand.print_operand k;
        print_newline ()
  done

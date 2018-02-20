type t = int array

let update (op, _, _, _) log =
  log.(Operator.num_of_op op) <- log.(Operator.num_of_op op) + 1;
  log

let print_log log =
  Array.iteri
    (fun i num ->
       Operator.print_operator (Operator.op_of_num i);
       Printf.eprintf ", %d\n" num)
    log

let init () = Array.make 49 0

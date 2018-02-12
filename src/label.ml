(* ラベルと行数の組 *)
type t = (string * int32) list

let global : t ref = ref []

let find_label str =
  try
    List.assoc str !global
  with Not_found ->
    failwith ("parser: label definition not found: " ^ str)

let print_label x =
  List.iter
    (fun (str, num) -> Printf.printf "%8s : %5ld\n" str num)
    x

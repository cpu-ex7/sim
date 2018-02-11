(* ラベルと行数の組 *)
type t = (string * int32) list

let global : t ref = ref []

let find_label str =
  try
    List.assoc str !global
  with Not_found ->
    failwith ("parser: label definition not found: " ^ str)

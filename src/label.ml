(* ラベルと行数の組 *)
type t = (string * int) list
(* グローバル変数 *)
let (g_label : t ref) = ref []

(* パーサーにグローバル変数を渡すことができないので、このモジュールから渡す *)
type label = (string * int) list
let (g_label:label ref) = ref []
let update_label label = g_label := label

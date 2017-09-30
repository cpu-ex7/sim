(* パーサーに変数を渡すことができないので、このモジュールから渡す *)
let (g_label:OpSyntax.label ref) = ref []
let (g_lines:OpSyntax.lines ref) = ref [||]
let update_label label = g_label := label
let update_lines lines = g_lines := lines

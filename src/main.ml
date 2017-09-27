open OpSyntax

type core = {
  reg : int array
}

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let doit lst =
  if List.length lst = 0
  then
    Parser.toplevel Lexer.main (Lexing.from_channel stdin)
  else
    let filename = List.hd lst in
    Parser.toplevel Lexer.main (Lexing.from_string filename)

{
  open Lexing
  open LabelParser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_' '$' ',' '-' '(' ')' '@']
let ident = (alpha | digit)*
let space = ' ' | '\t' | '\r'
let comment = '#'[^'\n']*'\n'

rule main = parse
| space+          { main lexbuf }
| comment         { new_line lexbuf; NEWLINE }
| "\n"            { new_line lexbuf; NEWLINE }
| ":"             { COLON }
| ident as id     { VAR id }
| eof             { EOF }
| _ {
  failwith ("LabelParser: Unknown token in line " ^
           (string_of_int lexbuf.lex_curr_p.pos_lnum)  ^
           ": " ^ Lexing.lexeme lexbuf)
  }

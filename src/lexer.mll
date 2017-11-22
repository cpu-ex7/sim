{
  open Lexing
  open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*
let space = ' ' | '\t' | '\r'
let comment = '#'[^'\n']*'\n'

rule main = parse
| space+          { main lexbuf }
| comment         { new_line lexbuf; NEWLINE }
| "\n"            { new_line lexbuf; NEWLINE }
| ","             { COMMA }
| ":"             { COLON }
| "-"             { MINUS }
| "("             { LPAREN }
| ")"             { RPAREN }
| '$'"zero"       { REG 0 }
| '$'"at"         { REG 1 }
| '$'"v0"         { REG 2 }
| '$'"v1"         { REG 3 }
| '$'"a0"         { REG 4 }
| '$'"a1"         { REG 5 }
| '$'"a2"         { REG 6 }
| '$'"a3"         { REG 7 }
| '$'"t0"         { REG 8 }
| '$'"t1"         { REG 9 }
| '$'"t2"         { REG 10 }
| '$'"t3"         { REG 11 }
| '$'"t4"         { REG 12 }
| '$'"t5"         { REG 13 }
| '$'"t6"         { REG 14 }
| '$'"t7"         { REG 15 }
| '$'"s0"         { REG 16 }
| '$'"s1"         { REG 17 }
| '$'"s2"         { REG 18 }
| '$'"s3"         { REG 19 }
| '$'"s4"         { REG 20 }
| '$'"s5"         { REG 21 }
| '$'"s6"         { REG 22 }
| '$'"s7"         { REG 23 }
| '$'"t8"         { REG 24 }
| '$'"t9"         { REG 25 }
| '$'"k0"         { REG 26 }
| '$'"k1"         { REG 27 }
| '$'"gp"         { REG 28 }
| '$'"sp"         { REG 29 }
| '$'"fp"         { REG 30 }
| '$'"ra"         { REG 31 }
| digit+ as n     { NUM (int_of_string n) }
| ident as id     { VAR id  }
| eof             { EOF }
| _ {
  failwith ("Parser: Unknown token in line " ^
           (string_of_int lexbuf.lex_curr_p.pos_lnum)  ^
           ": " ^ Lexing.lexeme lexbuf)
  }

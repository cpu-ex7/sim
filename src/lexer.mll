{
  open Lexing
  open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_' '@']
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
| '$'"zero"       { REG (Int32.of_int 0) }
| '$'"at"         { REG (Int32.of_int 1) }
| '$'"v0"         { REG (Int32.of_int 2) }
| '$'"v1"         { REG (Int32.of_int 3) }
| '$'"a0"         { REG (Int32.of_int 4) }
| '$'"a1"         { REG (Int32.of_int 5) }
| '$'"a2"         { REG (Int32.of_int 6) }
| '$'"a3"         { REG (Int32.of_int 7) }
| '$'"t0"         { REG (Int32.of_int 8) }
| '$'"t1"         { REG (Int32.of_int 9) }
| '$'"t2"         { REG (Int32.of_int 10) }
| '$'"t3"         { REG (Int32.of_int 11) }
| '$'"t4"         { REG (Int32.of_int 12) }
| '$'"t5"         { REG (Int32.of_int 13) }
| '$'"t6"         { REG (Int32.of_int 14) }
| '$'"t7"         { REG (Int32.of_int 15) }
| '$'"s0"         { REG (Int32.of_int 16) }
| '$'"s1"         { REG (Int32.of_int 17) }
| '$'"s2"         { REG (Int32.of_int 18) }
| '$'"s3"         { REG (Int32.of_int 19) }
| '$'"s4"         { REG (Int32.of_int 20) }
| '$'"s5"         { REG (Int32.of_int 21) }
| '$'"s6"         { REG (Int32.of_int 22) }
| '$'"s7"         { REG (Int32.of_int 23) }
| '$'"t8"         { REG (Int32.of_int 24) }
| '$'"t9"         { REG (Int32.of_int 25) }
| '$'"k0"         { REG (Int32.of_int 26) }
| '$'"k1"         { REG (Int32.of_int 27) }
| '$'"gp"         { REG (Int32.of_int 28) }
| '$'"sp"         { REG (Int32.of_int 29) }
| '$'"fp"         { REG (Int32.of_int 30) }
| '$'"ra"         { REG (Int32.of_int 31) }
| '$'"f" digit+ as str { FREG (Int32.of_string (String.sub str 2 ((String.length str) - 2))) }
| digit+ as n     { NUM (Int32.of_string n) }
| ident as id     { VAR id  }
| eof             { EOF }
| _ {
  failwith ("Parser: Unknown token in line " ^
           (string_of_int lexbuf.lex_curr_p.pos_lnum)  ^
           ": " ^ Lexing.lexeme lexbuf)
  }

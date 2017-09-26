let space = ' ' | '\t' | '\r' | '\n'

rule main = parse
| space+          { main lexbuf }
| "add"           { Parser.OP "add" }
| ","             { Parser.COMMA }
| '$'"zero"       { Parser.REG 0 }
| '$'"at"         { Parser.REG 1 }
| '$'"v0"         { Parser.REG 2 }
| '$'"v1"         { Parser.REG 3 }
| '$'"a0"         { Parser.REG 4 }
| '$'"a1"         { Parser.REG 5 }
| '$'"a2"         { Parser.REG 6 }
| '$'"a3"         { Parser.REG 7 }
| '$'"t0"         { Parser.REG 8 }
| '$'"t1"         { Parser.REG 9 }
| '$'"t2"         { Parser.REG 10 }
| '$'"t3"         { Parser.REG 11 }
| '$'"t4"         { Parser.REG 12 }
| '$'"t5"         { Parser.REG 13 }
| '$'"t6"         { Parser.REG 14 }
| '$'"t7"         { Parser.REG 15 }
| '$'"s0"         { Parser.REG 16 }
| '$'"s1"         { Parser.REG 17 }
| '$'"s2"         { Parser.REG 18 }
| '$'"s3"         { Parser.REG 19 }
| '$'"s4"         { Parser.REG 20 }
| '$'"s5"         { Parser.REG 21 }
| '$'"s6"         { Parser.REG 22 }
| '$'"s7"         { Parser.REG 23 }
| '$'"t8"         { Parser.REG 24 }
| '$'"t9"         { Parser.REG 25 }
| '$'"k0"         { Parser.REG 26 }
| '$'"k1"         { Parser.REG 27 }
| '$'"gp"         { Parser.REG 28 }
| '$'"sp"         { Parser.REG 29 }
| '$'"fp"         { Parser.REG 30 }
| '$'"ra"         { Parser.REG 31 }
| eof           { Parser.EOF }
| _             { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}

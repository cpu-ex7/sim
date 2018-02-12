%{
  open Operand
%}

%token COLON
%token NEWLINE
%token COMMA
%token MINUS
%token LPAREN
%token RPAREN
%token <int32> REG
%token <int32> FREG
%token <int32> NUM
%token <string> VAR
%token EOF

%start toplevel
%type <Program.t> toplevel
%%

toplevel:
  | line_list EOF { $1 }
  | EOF { [||] }

line_list:
  | op line_list            { Array.append [|$1|] $2 }
  | others line_list        { $2 }
  | op                      { [|$1|] }
  | others                  { [||] }

others:
  | VAR COLON NEWLINE      { }
  | NEWLINE                { }

op:
  | VAR operands NEWLINE   { Operator.of_string $1, $2 }

operands:
  | operand COMMA operand COMMA operand { ($1, $3, $5) }
  | operand COMMA operand               { ($1, $3, Empty) }
  | operand                             { ($1, Empty, Empty) }
  |                                     { (Empty, Empty, Empty) }

operand:
  | REG                    { Reg $1 }
  | FREG                   { FReg $1 }
  | NUM                    { Imm $1 }
  | MINUS NUM              { Imm (Int32.neg $2) }
  | NUM LPAREN REG RPAREN  { RelReg ($1, $3) }
  | VAR { Rabel $1 }

%{
  open OpSyntax
%}

%token COLON
%token NEWLINE
%token COMMA
%token MINUS
%token LPAREN
%token RPAREN
%token <int> REG
%token <int> NUM
%token <string> VAR
%token EOF

%start toplevel
%type <OpSyntax.lines> toplevel
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
  | VAR operands NEWLINE   { OpSyntax.lookup $1, $2 }

operands:
  | operand COMMA operand COMMA operand { ($1, $3, $5) }
  | operand COMMA operand               { ($1, $3, Empty) }
  | operand                             { ($1, Empty, Empty) }
  |                                     { (Empty, Empty, Empty) }
 
operand:
  | REG                    { Reg $1 }
  | NUM                    { Imm $1 }
  | MINUS NUM              { Imm (-$2) }
  | NUM LPAREN REG RPAREN  { RelReg ($1, $3) }
  | VAR {
     try Dest (List.assoc $1 !(Program.g_label))
     with Not_found -> failwith ("parser: label definition not found: " ^ $1)
   }

%{

%}

%token COLON
%token NEWLINE
%token COMMA
%token MINUS
%token <int> REG
%token <int> IMM
%token <string> VAR
%token EOF

%start toplevel
%type <OpSyntax.t> toplevel
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
  | operand COMMA operands { Array.append [|$1|] $3 }
  | operand                { [|$1|] }
  |                        { [||] }

operand:
  | REG                    { Reg $1 }
  | IMM                    { Imm $1 }
  | MINUS IMM              { Imm (-$2) }
  | VAR {
     try Dest (List.assoc $1 !(Label.g_label))
     with Not_found -> failwith ("parser: label definition not found: " ^ $1)
   }

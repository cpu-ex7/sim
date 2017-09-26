%{
  open OpSyntax
%}

%token COMMA
%token <int> REG
%token <string> OP
%token EOF

%start toplevel
%type <OpSyntax.t list> toplevel
%%

toplevel: op_list EOF      { $1 } ;

op_list:
  | op op_list             { $1::$2 }
  | op                     { [$1] }

op:
  | OP operands            { lookup $1 $2 }

operands:
  | operand COMMA operands { $1 :: $3 }
  | operand                { [$1] }

operand:
  | REG                    { $1 }

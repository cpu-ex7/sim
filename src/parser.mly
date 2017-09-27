%{
  open OpSyntax
  let num_ops = ref 0
  let labels = ref []

  let new_op () =
    num_ops := !num_ops + 1
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
  | line_list EOF {
      let l = !labels in
      labels := []; (* 次回の使用に備えて変数をリセットする *)
      num_ops := 0;
      { ops = $1; labels = l}
    }
  | EOF { { ops = [||]; labels = [] } }

line_list:
  | op line_list            { Array.append [|$1|] $2 }
  | others line_list        { $2 }
  | op                      { [|$1|] }
  | others                  { [||] }

others:
  | VAR COLON NEWLINE      { labels := ($1, !num_ops) :: !labels }
  | NEWLINE                { }

op:
  | VAR operands NEWLINE   { new_op (); lookup $1, $2 }
  | VAR                    { new_op (); lookup $1, [||] }

operands:
  | operand COMMA operands { Array.append [|$1|] $3 }
  | operand                { [|$1|] }

operand:
  | REG                    { Reg $1 }
  | IMM                    { Imm $1 }
  | MINUS IMM              { Imm (-$2) }
  | VAR                    { Dest $1 }

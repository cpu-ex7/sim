%{
  let num_ops = ref 0
  let labels = ref []

  let new_op () =
    num_ops := !num_ops + 1
%}

%token COLON
%token NEWLINE
%token <string> VAR
%token EOF

%start toplevel
%type <Label.t> toplevel
%%

toplevel:
  | line_list EOF {
      let l = !labels in
      labels := []; (* 次回の使用に備えて変数をリセットする *)
      num_ops := 0;
      l
    }
  | EOF { [] }

line_list:
  | label line_list        { $2 }
  | others line_list       { Array.append [|$1|] $2 }

| label                  { [||] }
  | others                 { [|$1|] }

label:
  | VAR COLON NEWLINE      { labels := ($1, !num_ops) :: !labels }

others:
  | vars NEWLINE          { new_op () }
  | NEWLINE               { }

vars:
  | VAR vars              { }
  | VAR                   { }

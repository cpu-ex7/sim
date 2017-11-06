(* 命令の型 *)
type t =
  (* 算術命令 *)
  | OpAdd
  | OpSub
  | OpAddi
  (* 論理命令 *)
  | OpAnd
  | OpOr
  | OpNor
  | OpXor
  (* シフト命令 *)
  | OpSll
  | OpSllv
  | OpSrl
  | OpSrlv
  | OpSra
  | OpSrav
  (* 制御命令 *)
  | OpSlt
  | OpSlti
  | OpJump
  | OpJal
  | OpJr
  | OpBne
  | OpBeq
  | OpHalt
  (* float命令 *)
  | OpLwc1
  | OpLwc2
  | OpSwc1
  | OpSwc2
  | OpAddf
  | OpSubf
  | OpMulf
  | OpDivf
  | OpSqrt
  | OpAbs
  (* メモリ命令 *)
  | OpLi
  | OpLui
  | OpLw
  | OpSw

(* アセンブリの文字列とoperator型の値との関係 *)
let op_alist =
  [ (* 算術命令 *)
    ("add", OpAdd);
    ("sub", OpSub);
    ("addi", OpAddi);
    (* 論理命令 *)
    ("and", OpAnd);
    ("or", OpOr);
    ("nor", OpNor);
    ("xor", OpXor);
    (* シフト命令 *)
    ("sll", OpSll);
    ("sllv", OpSllv);
    ("srl", OpSrl);
    ("srlv", OpSrlv);
    ("sra", OpSrl);
    ("srav", OpSrlv);
    (* 制御命令 *)
    ("slt", OpSlt);
    ("slti", OpSlti);
    ("j", OpJump);
    ("jal", OpJal);
    ("jr", OpJr);
    ("bne", OpBne);
    ("beq", OpBeq);
    ("halt", OpHalt);
    (* float命令 *)
    ("lwc1", OpLwc1);
    ("swc1", OpSwc1);
    ("lwc2", OpLwc2);
    ("swc2", OpSwc2);
    ("addf", OpAddf);
    ("subf", OpSubf);
    ("mulf", OpMulf);
    ("divf", OpDivf);
    ("sqrt", OpSqrt);
    ("abs", OpAbs);
    (* メモリ命令 *)
    ("li", OpLi);
    ("lui", OpLui);
    ("lw", OpLw);
    ("sw", OpSw);]

(* op_alistの逆 *)
let op_alist_rev =
  List.map (fun (x, y) -> (y, x)) op_alist

(* op_strに対応するOperator.t型の値を返す *)
let of_string op_str = List.assoc op_str op_alist

(* 命令の型 *)
type t =
  (* 算術命令 *)
  | OpAdd
  | OpSub
  | OpAddi
  (* 論理命令 *)
  | OpAnd
  | OpOr
  | OpOri
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
  | OpJalr
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
  | OpMvf
  (* float変換命令 *)
  | OpMfc1
  | OpMfc2
  | OpRevn
  | OpCvtsw
  (* float比較命令 *)
  | OpEqf
  | OpNef
  | OpLtf
  | OpLef
  | OpGtf
  | OpGef
  (* float制御命令 *)
  | OpBct
  | OpBcf
  (* メモリ命令 *)
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
    ("ori", OpOri);
    ("nor", OpNor);
    ("xor", OpXor);
    (* シフト命令 *)
    ("sll", OpSll);
    ("sllv", OpSllv);
    ("srl", OpSrl);
    ("srlv", OpSrlv);
    ("sra", OpSra);
    ("srav", OpSrav);
    (* 制御命令 *)
    ("slt", OpSlt);
    ("slti", OpSlti);
    ("j", OpJump);
    ("jal", OpJal);
    ("jalr", OpJalr);
    ("jr", OpJr);
    ("bne", OpBne);
    ("beq", OpBeq);
    ("halt", OpHalt);
    (* float命令 *)
    ("lwc1", OpLwc1);
    ("swc1", OpSwc1);
    ("lwc2", OpLwc2);
    ("read_word", OpLwc2);
    ("swc2", OpSwc2);
    ("print_char", OpSwc2);
    ("addf", OpAddf);
    ("subf", OpSubf);
    ("mulf", OpMulf);
    ("divf", OpDivf);
    ("sqrt", OpSqrt);
    ("abs", OpAbs);
    ("mvf", OpMvf);
    (* float変換命令 *)
    ("mfc1", OpMfc1);
    ("mfc2", OpMfc2);
    ("roundwfmt", OpRevn);
    ("cvtsw", OpCvtsw);
    (* float比較命令 *)
    ("eqf", OpEqf);
    ("nef", OpNef);
    ("ltf", OpLtf);
    ("lef", OpLef);
    ("gtf", OpGtf);
    ("gef", OpGef);
    (* float比較命令 *)
    ("bc1t", OpBct);
    ("bc1f", OpBcf);
    (* メモリ命令 *)
    ("lui", OpLui);
    ("lw", OpLw);
    ("sw", OpSw);]

let num_of_op = function
  (* 算術命令 *)
  | OpAdd -> 0
  | OpSub -> 1
  | OpAddi -> 2
  (* 論理命令 *)
  | OpAnd -> 3
  | OpOr -> 4
  | OpOri -> 5
  | OpNor -> 6
  | OpXor -> 7
  (* シフト命令 *)
  | OpSll -> 8
  | OpSllv -> 9
  | OpSrl -> 10
  | OpSrlv -> 11
  | OpSra -> 12
  | OpSrav -> 13
  (* 制御命令 *)
  | OpSlt -> 14
  | OpSlti -> 15
  | OpJump -> 16
  | OpJal -> 17
  | OpJalr -> 18
  | OpJr -> 19
  | OpBne -> 20
  | OpBeq -> 21
  | OpHalt -> 22
  (* float命令 *)
  | OpLwc1 -> 23
  | OpLwc2 -> 24
  | OpSwc1 -> 25
  | OpSwc2 -> 26
  | OpAddf -> 27
  | OpSubf -> 28
  | OpMulf -> 29
  | OpDivf -> 30
  | OpSqrt -> 31
  | OpAbs -> 32
  | OpMvf -> 33
  (* float変換命令 *)
  | OpMfc1 -> 34
  | OpMfc2 -> 35
  | OpRevn -> 36
  | OpCvtsw -> 37
  (* float比較命令 *)
  | OpEqf -> 38
  | OpNef -> 39
  | OpLtf -> 40
  | OpLef -> 41
  | OpGtf -> 42
  | OpGef -> 43
  (* float制御命令 *)
  | OpBct -> 44
  | OpBcf -> 45
  (* メモリ命令 *)
  | OpLui -> 46
  | OpLw -> 47
  | OpSw -> 48

let op_of_num = function
  (* 算術命令 *)
  | 0 -> OpAdd
  | 1 -> OpSub
  | 2 -> OpAddi
  (* 論理命令 *)
  | 3 -> OpAnd
  | 4 -> OpOr
  | 5 -> OpOri
  | 6 -> OpNor
  | 7 -> OpXor
  (* シフト命令 *)
  | 8 -> OpSll
  | 9 -> OpSllv
  | 10 -> OpSrl
  | 11 -> OpSrlv
  | 12 -> OpSra
  | 13 -> OpSrav
  (* 制御命令 *)
  | 14 -> OpSlt
  | 15 -> OpSlti
  | 16 -> OpJump
  | 17 -> OpJal
  | 18 -> OpJalr
  | 19 -> OpJr
  | 20 -> OpBne
  | 21 -> OpBeq
  | 22 -> OpHalt
  (* float命令 *)
  | 23 -> OpLwc1
  | 24 -> OpLwc2
  | 25 -> OpSwc1
  | 26 -> OpSwc2
  | 27 -> OpAddf
  | 28 -> OpSubf
  | 29 -> OpMulf
  | 30 -> OpDivf
  | 31 -> OpSqrt
  | 32 -> OpAbs
  | 33 -> OpMvf
  (* float変換命令 *)
  | 34 -> OpMfc1
  | 35 -> OpMfc2
  | 36 -> OpRevn
  | 37 -> OpCvtsw
  (* float比較命令 *)
  | 38 -> OpEqf
  | 39 -> OpNef
  | 40 -> OpLtf
  | 41 -> OpLef
  | 42 -> OpGtf
  | 43 -> OpGef
  (* float制御命令 *)
  | 44 -> OpBct
  | 45 -> OpBcf
  (* メモリ命令 *)
  | 46 -> OpLui
  | 47 -> OpLw
  | 48 -> OpSw
  | _ -> raise Not_found

(* op_alistの逆 *)
let op_alist_rev =
  List.map (fun (x, y) -> (y, x)) op_alist

let print_operator x =
  Printf.eprintf "%10s" (List.assoc x op_alist_rev)

(* op_strに対応するOperator.t型の値を返す *)
let of_string op_str =
  try
    List.assoc op_str op_alist
  with Not_found ->
    failwith @@ "operand not found : " ^ op_str

type t = {
  pc : int ref;       (* プログラムカウンタ *)
  cc : bool ref;       (* コンディションレジスタ($cc) *)
  reg : int array;    (* 整数レジスタ *)
  freg : float array; (* floatレジスタ *)
  mem : int array;    (* メモリ *)
  (* floatメモリ: 型の整合性を取るのが難しいので、int/floatでメモリを分ける *)
  (* 2つのメモリ間の同期は取らない *)
  fmem : float array;
  count : int ref;    (* ログ *)
}

let empty_core () =
  let core =
    {
      pc = ref 0;
      cc = ref false;
      reg = Array.make 32 0;
      freg = Array.make 32 0.0;
      mem = Array.make 8192 0;    (* 一応 8192 にしておく、増やす必要? *)
      fmem = Array.make 8192 0.0; (* 一応 8192 にしておく、増やす必要? *)
      count = ref 0
    } in
  (* fp, spの初期値は適当 *)
  core.reg.(Operand.regnum_of_string "$fp") <- 4096;
  core.reg.(Operand.regnum_of_string "$sp") <- 4096;
  (* $raの初期値を-1にして、 "jr $ra"で終わるコード片を実行した時に
     index_out_of_bounds例外がでて実行が終わるようにする *)
  core.reg.(Operand.regnum_of_string "$ra") <- -1;
  core

(* グローバルのコア *)
let g_core = ref @@ empty_core ()

(* グローバルのコアに空のコアをロードする *)
let load_empty_core () = g_core := empty_core ()

(* 引数用レジスタにnumsをセットする *)
let set_args args =
  assert ((List.length args) <= 4);
  List.iteri
    (fun i n ->
       !g_core.reg.(Operand.regnum_of_string "$a0" + i) <- n)
    args

let set_fp n =
  !g_core.reg.(Operand.regnum_of_string "$fp") <- n

let set_sp n =
  !g_core.reg.(Operand.regnum_of_string "$sp") <- n

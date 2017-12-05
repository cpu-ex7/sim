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

let empty () =
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

(* 引数用レジスタにnumsをセットする *)
let set_args core args =
  assert ((List.length args) <= 4);
  List.iteri
    (fun i n ->
       !core.reg.(Operand.regnum_of_string "$a0" + i) <- n)
    args

let set_register_name core str n =
  !core.reg.(Operand.regnum_of_string str) <- n
let set_register_idx core idx n =
  !core.reg.(idx) <- n

(* from番地からnum個のメモリの内容を表示 *)
let dump_memory core from num =
  print_endline "memory (int)";
  for i = from to from + num do
    Printf.printf "mem[%d] -> %d" i (!core.mem.(i))
  done;
  print_endline "memory (float)";
  for i = from to from + num do
    Printf.printf "smem[%d] -> %f" i (!core.fmem.(i))
  done

(* from番地からi個のメモリの内容を返す *)
let dump_memory_alist core from num =
  let ans = ref [] in
  for i = from to from + num do
    ans := (i, !core.mem.(i)) :: !ans
  done;
  let fans = ref [] in
  for i = from to from + num do
    fans := (i, !core.fmem.(i)) :: !fans
  done;
  !ans, !fans

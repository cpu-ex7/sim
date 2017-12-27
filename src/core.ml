open Int32
open Printf

type t = {
  pc : int32 ref;       (* プログラムカウンタ *)
  cc : bool ref;       (* コンディションレジスタ($cc) *)
  reg : int32 array;    (* 整数レジスタ *)
  freg : int32 array; (* floatレジスタ *)
  mem : int32 array;    (* メモリ *)
  input_string : string ref;
  input_index : int ref;
  count : int ref;    (* ログ *)
}

let print_core c =
  printf "pc : %08lx\n" !(c.pc);
  printf "cc : %B\n" !(c.cc);
  for i = 0 to 31 do
    printf "reg%02d  : %08lx\n" i (c.reg).(i);
  done;
  for i = 0 to 31 do
    printf "freg%02d : %08lx\n" i (c.freg).(i);
    (*printf "freg%02d : %016Lx\n" i (c.freg).(i);*) (*倍精度で表示したい場合*)
  done;
  printf "input_string : %s\n" !(c.input_string);
  printf "input_index : %08x\n" !(c.input_index);
  printf "count : %08x\n" !(c.count)

let empty () =
  let core =
    {
      pc = ref Int32.zero;
      cc = ref false;
      reg = Array.make 32 Int32.zero;
      freg = Array.make 32 Int32.zero;
      mem = Array.make 8192 Int32.zero;    (* 一応 8192 にしておく、増やす必要? *)
      input_string = ref "";
      input_index = ref 0;
      count = ref 0
    } in
  (* fp, spの初期値は適当 *)
  core.reg.(Operand.regnum_of_string "$fp") <- of_int 4096;
  core.reg.(Operand.regnum_of_string "$sp") <- of_int 4096;
  (* $raの初期値を-1にして、 "jr $ra"で終わるコード片を実行した時に
     index_out_of_bounds例外がでて実行が終わるようにする *)
  core.reg.(Operand.regnum_of_string "$ra") <- of_int ~-1;
  core

(* 引数用レジスタにnumsをセットする *)
let set_args core args =
  assert ((List.length args) <= 4);
  List.iteri
    (fun i n ->
       !core.reg.(Operand.regnum_of_string "$a0" + i) <- of_int n)
    args

let set_register_name core str n =
  !core.reg.(Operand.regnum_of_string str) <- n
let set_register_idx core idx n =
  !core.reg.(idx) <- n

(* from番地からnum個のメモリの内容を表示 *)
let dump_memory core from num =
  print_endline "memory (int)";
  for i = from to from + num do
    Printf.printf "mem[%d] -> %d\n" i (to_int !core.mem.(i))
  done

(* from番地からi個のメモリの内容を返す *)
let dump_memory_alist core from num =
  let ans = ref [] in
  for i = from to from + num do
    ans := (i, !core.mem.(i)) :: !ans
  done;
  !ans

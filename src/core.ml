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
  log : Log.t ref;
}

let print_core c =
  printf "pc     : %08lx %10ld\n" !(c.pc) !(c.pc);
  printf "cc     : %B\n" !(c.cc);
  for i = 0 to 31 do
    printf "reg%02d(%5s)  : %08lx %10ld\n"
      i
      (Operand.string_of_regnum i)
      (c.reg).(i)
      (c.reg).(i);
  done;
  for i = 0 to 31 do
    printf "freg%02d : %08lx %19.8f\n" i (c.freg).(i) (Int32.float_of_bits (c.freg).(i));
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
      mem = Array.make 131072 Int32.zero;
      input_string = ref "";
      input_index = ref 0;
      count = ref 0;
      log = ref (Log.init ());
    } in
  (* fp, spの初期値は適当 *)
  core.reg.(Operand.regnum_of_string "$fp") <- of_int 0;
  core.reg.(Operand.regnum_of_string "$sp") <- of_int 0;
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

(* ユーティリティ関数 *)
let regnum_lr = 31l
let pc core = !(core.pc)
let next_pc core = add !(core.pc) one
let incr core = core.pc := add !(core.pc) one
let jump core i = core.pc := i
let rget core i = core.reg.(to_int i)
let rset core i n = core.reg.(to_int i) <- n
let mget core i = core.mem.(to_int i)
let mset core i j = core.mem.(to_int i) <- j
let rgetf core i = core.freg.(to_int i)
let rsetf core i n = core.freg.(to_int i) <- n
let cget core  = !(core.cc)
let cset core b = core.cc := b
let get_input core =
  core.input_index := !(core.input_index) + 1;
  !(core.input_string).[!(core.input_index) - 1] |> Char.code |> of_int

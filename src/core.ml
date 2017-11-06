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

(* グローバルのコア *)
let g_core =
  ref {
    pc = ref 0;
    cc = ref false;
    reg = Array.make 32 0;
    freg = Array.make 32 0.0;
    mem = Array.make 8192 0;    (* 一応 8192 にしておく、増やす必要? *)
    fmem = Array.make 8192 0.0; (* 一応 8192 にしておく、増やす必要? *)
    count = ref 0
  }


(* グローバルのコアに空のコアをロードする *)
let load_empty_core () = g_core :=
    {
      pc = ref 0;
      cc = ref false;
      reg = Array.make 32 0;
      freg = Array.make 32 0.0;
      mem = Array.make 8192 0;    (* 一応 8192 にしておく、増やす必要? *)
      fmem = Array.make 8192 0.0; (* 一応 8192 にしておく、増やす必要? *)
      count = ref 0
    }

type t = {
  pc : int ref;
  reg : int array;
  mem : int array;
  count : int ref; (* ログ *)
}

let g_core =
  ref {
    pc = ref 0;
    reg = Array.make 32 0;
    mem = Array.make 8192 0; (* 一応 8192 にしておく、増やす必要? *)
    count = ref 0
  }

let load_empty_core () = g_core :=
    {
      pc = ref 0;
      reg = Array.make 32 0;
      mem = Array.make 8192 0; (* 一応 8192 にしておく、増やす必要? *)
      count = ref 0
    }

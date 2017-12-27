open Int32

external addf32 : int32 -> int32 -> int32 = "addf32"
external subf32 : int32 -> int32 -> int32 = "subf32"
external mulf32 : int32 -> int32 -> int32 = "mulf32"
external divf32 : int32 -> int32 -> int32 = "divf32"
external sqrt32 : int32 -> int32 = "sqrt32"
external abs_float32 : int32 -> int32 = "abs_float32"
external round_even32 : int32 -> int32 = "round_even32"
external float32_of_int : int32 -> int32 = "float32_of_int"

let lower_bits_of_float f =
  f |> bits_of_float |> logand (of_int 65535)

let upper_bits_of_float f =
  shift_right_logical (sub (bits_of_float f)  (lower_bits_of_float f)) 16

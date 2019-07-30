type t =
| Int
| Int64
| Float64
| String of int
| Bigstring
| Array_float32
| Array_float64

val size : t -> int

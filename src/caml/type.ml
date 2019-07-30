type t =
| Int
| Int64
| Float64
| String of int
| Bigstring
| Array_float32
| Array_float64

let size = function
| Int | Int64 | Float64 | Bigstring -> 8
| String l -> l
| Array_float32 | Array_float64 -> 16

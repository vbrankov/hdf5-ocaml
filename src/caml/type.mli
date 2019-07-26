type t =
| Int
| Int64
| Float64
| String of int
| Bigstring

val size : t -> int

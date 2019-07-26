type t =
| Int
| Int64
| Float64
| String of int
| Bigstring

let size = function
| Int | Int64 | Float64 | Bigstring -> 8
| String l -> l

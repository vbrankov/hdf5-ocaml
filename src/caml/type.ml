type t =
| Int
| Int64
| Float64
| String of int

let size = function
| Int | Int64 | Float64 -> 8
| String l -> l

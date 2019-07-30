type t =
| Int
| Int64
| Float64
| String of int
| Bigstring
| Array_float32
| Array_float64
| Array_sint8
| Array_uint8
| Array_sint16
| Array_uint16
| Array_int32
| Array_int64
| Array_int
| Array_nativeint
| Array_char

let size = function
| Int | Int64 | Float64 | Bigstring -> 8
| String l -> l
| Array_float32
| Array_float64
| Array_sint8
| Array_uint8
| Array_sint16
| Array_uint16
| Array_int32
| Array_int64
| Array_int
| Array_nativeint
| Array_char -> 16

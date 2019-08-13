open Bigarray

type t

external int_as_pointer : 'a -> 'b = "%int_as_pointer"
let null = int_as_pointer 0
external of_string : string -> t = "hdf5_c_string_of_string"
external to_string : t -> string = "caml_copy_string"
external to_bigstring : t -> (char, int8_unsigned_elt, c_layout) Array1.t
  = "hdf5_c_string_to_bigstring"
external free : t -> unit = "free"

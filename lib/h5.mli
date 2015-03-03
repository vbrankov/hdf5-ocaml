module Hsize : sig
  type t

  val of_int : int -> t
  external to_int : t -> int = "%identity"
  val of_int_array : int array -> t array
  val unlimited : t
end

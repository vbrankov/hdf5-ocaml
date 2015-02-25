module Hsize : sig
  type t

  val of_int : int -> t
  external to_int : t -> int = "%identity"
  val unlimited : t
end

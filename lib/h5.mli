module Hsize : sig
  type t

  external of_int : int -> t = "%identity"
  val unlimited : t
end

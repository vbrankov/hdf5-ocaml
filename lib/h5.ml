module Hsize = struct
  type t

  let of_int =
    let negative = Invalid_argument "Hsize.of_int: negative value" in
    fun i -> if i < 0 then raise negative
  external to_int : t -> int = "%identity"
  let unlimited = -1
end

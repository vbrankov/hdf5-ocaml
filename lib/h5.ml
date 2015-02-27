module Hsize = struct
  type t = int

  let of_int =
    let negative = Invalid_argument "Hsize.of_int: negative value" in
    fun i -> if i < 0 then raise negative; i
  external to_int : t -> int = "%identity"
  let unlimited = -1
end

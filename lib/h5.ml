module Hsize = struct
  type t = int

  let negative = Invalid_argument "Hsize.of_int: negative value"
  let of_int =
    fun i -> if i < 0 then raise negative; i
  external to_int : t -> int = "%identity"
  let of_int_array a =
    for i = 0 to Array.length a - 1 do
      if a.(i) < 0 then raise negative
    done;
    a
  let unlimited = -1
end

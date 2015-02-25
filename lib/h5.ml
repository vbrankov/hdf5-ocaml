module Hsize = struct
  type t

  external of_int : int -> t = "%identity"
  external get_unlimited : unit -> t = "hdf5_h5_get_unlimited_stub"
  let unlimited = get_unlimited ()
end

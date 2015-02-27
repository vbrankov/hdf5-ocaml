type t

module Loc = struct
  type t =
  [ `File of H5f.t
  | `Group of t ]
end

external close : t -> unit = "hdf5_h5p_close"
external create : Loc.t -> ?lcpl:H5p.t -> ?gcpl:H5p.t -> ?gapl:H5p.t -> string -> t
  = "hdf5_h5g_create"
external open_ : Loc.t -> ?gapl:H5p.t -> string -> t = "hdf5_h5g_open"

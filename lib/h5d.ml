type t

external create : Loc.t -> string -> H5t.t -> ?lcpl:H5p.t -> ?dcpl:H5p.t -> ?apl:H5p.t
  -> H5s.t -> t = "hdf5_h5d_create_bytecode" "hdf5_h5d_create"

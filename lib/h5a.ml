type t

external create : Loc.t -> string -> H5t.t -> ?acpl:H5p.t -> ?aapl:H5p.t -> H5d.t -> t
  = "hdf5_h5a_create_bytecode" "hdf5_h5a_create"

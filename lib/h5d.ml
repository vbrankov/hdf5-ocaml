open Bigarray

type t

external create : Loc.t -> string -> H5t.t -> ?lcpl:H5p.t -> ?dcpl:H5p.t -> ?apl:H5p.t
  -> H5s.t -> t = "hdf5_h5d_create_bytecode" "hdf5_h5d_create"
external open_ : Loc.t -> ?dapl:H5p.t -> string -> t = "hdf5_h5d_open"
external close : t -> unit = "hdf5_h5d_close"
external get_space : t -> H5s.t = "hdf5_h5d_get_space"
external get_type : t -> H5t.t = "hdf5_h5d_get_type"
external read : t -> H5t.t -> mem_space:H5s.t -> file_space:H5s.t -> ?xfer_plist:H5p.t
  -> _ Genarray.t -> unit = "hdf5_h5d_read_bytecode" "hdf5_h5d_read"
external write : t -> H5t.t -> mem_space:H5s.t -> file_space:H5s.t -> ?xfer_plist:H5p.t
  -> _ Genarray.t -> unit = "hdf5_h5d_write_bytecode" "hdf5_h5d_write"

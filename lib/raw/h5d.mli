module Layout : sig
  type t =
  | COMPACT
  | CONTIGUOUS
  | CHUNKED
  | NLAYOUTS
end

module Alloc_time : sig
  type t =
  | DEFAULT
  | EARLY
  | LATE
  | INCR
end

module Space_status : sig
  type t =
  | NOT_ALLOCATED
  | PART_ALLOCATED
  | ALLOCATED
end

module Fill_time : sig
  type t =
  | ALLOC
  | NEVER
  | IFSET
end

module Fill_value : sig
  type t =
  | UNDEFINED
  | DEFAULT
  | USER_DEFAULT
end

external create : Hid.t -> string -> Hid.t -> ?lcpl:Hid.t -> ?dcpl:Hid.t -> ?apl: Hid.t
  -> Hid.t -> Hid.t = "hdf5_h5d_create_bytecode" "hdf5_h5d_create"
external create_anon : Hid.t -> Hid.t -> ?dcpl:Hid.t -> ?apl: Hid.t -> Hid.t -> Hid.t
  = "hdf5_h5d_create_anon"
external open_ : Hid.t -> ?dapl:Hid.t -> string -> Hid.t = "hdf5_h5d_open"
external close : Hid.t -> unit = "hdf5_h5d_close"
external get_space : Hid.t -> Hid.t = "hdf5_h5d_get_space"
external get_space_status : Hid.t -> Space_status.t = "hdf5_h5d_get_space_status"
external get_type : Hid.t -> Hid.t = "hdf5_h5d_get_type"
external get_create_plist : Hid.t -> Hid.t = "hdf5_h5d_get_create_plist"
external read : Hid.t -> Hid.t -> Hid.t -> Hid.t -> ?xfer_plist:Hid.t -> _ -> unit
  = "hdf5_h5d_read_bytecode" "hdf5_h5d_read"
external write : Hid.t -> Hid.t -> Hid.t -> Hid.t -> ?xfer_plist:Hid.t -> _ -> unit
  = "hdf5_h5d_write_bytecode" "hdf5_h5d_write"
external extend : Hid.t -> Hsize.t array -> unit = "hdf5_h5d_set_extent"
external set_extent : Hid.t -> Hsize.t array -> unit = "hdf5_h5d_set_extent"

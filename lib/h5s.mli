open H5

type t

module Class : sig
  type t =
  | NO_CLASS
  | SCALAR
  | SIMPLE
  | NULL
end

module Select : sig
  type t =
  | SET
  | OR
  | AND
  | XOR
  | NOTB
  | NOTA
end

val all : t

external create : Class.t -> t = "hdf5_h5s_create"
external close : t -> unit = "hdf5_h5s_close"
external create_simple : ?maximum_dims:Hsize.t array -> Hsize.t array -> t
  = "hdf5_h5s_create_simple"
external get_simple_extent_dims : t -> Hsize.t array * Hsize.t array
  = "hdf5_h5s_get_simple_extent_dims"
external select_elements : t -> Select.t -> Hsize.t array -> unit
  = "hdf5_h5s_select_elements"
external select_none : t -> unit = "hdf5_h5s_select_none"
external select_hyperslab : t -> Select.t -> start:Hsize.t array -> ?stride:Hsize.t array
  -> count:Hsize.t array -> ?block:Hsize.t array -> unit -> unit
  = "hdf5_h5s_select_hyperslab_bytecode" "hdf5_h5s_select_hyperslab"

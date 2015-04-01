type t

module Class = struct
  type t =
  | NO_CLASS
  | SCALAR
  | SIMPLE
  | NULL
end

module Select = struct
  type t =
  | SET
  | OR
  | AND
  | XOR
  | NOTB
  | NOTA
end

external get_all : unit -> t = "hdf5_h5s_get_all"
let all = get_all ()

external create : Class.t -> t = "hdf5_h5s_create"
external close : t -> unit = "hdf5_h5s_close"
external create_simple : ?maximum_dims:Hsize.t array -> Hsize.t array -> t
  = "hdf5_h5s_create_simple"
external get_simple_extent_dims : t -> Hsize.t array * Hsize.t array
  = "hdf5_h5s_get_simple_extent_dims"
external get_simple_extent_npoints : t -> int = "hdf5_h5s_get_simple_extent_npoints"
external set_extent_simple : t -> ?maximum_size:Hsize.t array -> Hsize.t array -> unit
  = "hdf5_h5s_set_extent_simple"
external get_select_npoints : t -> int = "hdf5_h5s_get_select_npoints"
external get_select_hyper_blocklist : ?startblock:int -> ?numblocks:int -> t
  -> Hsize.t array = "hdf5_h5s_get_select_hyper_blocklist"
external get_select_elem_pointlist : ?startblock:int -> ?numpoints:int -> t
  -> Hsize.t array = "hdf5_h5s_get_select_elem_pointlist"
external get_select_bounds : t -> int * int = "hdf5_h5s_get_select_bounds"
external select_elements : t -> Select.t -> Hsize.t array -> unit
  = "hdf5_h5s_select_elements"
external select_none : t -> unit = "hdf5_h5s_select_none"
external select_hyperslab : t -> Select.t -> start:Hsize.t array -> ?stride:Hsize.t array
  -> count:Hsize.t array -> ?block:Hsize.t array -> unit -> unit
  = "hdf5_h5s_select_hyperslab_bytecode" "hdf5_h5s_select_hyperslab"

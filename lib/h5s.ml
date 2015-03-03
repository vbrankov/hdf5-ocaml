open H5

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
external create_simple : ?maximum_dims:int array -> current_dims:Hsize.t array -> unit
  -> t = "hdf5_h5s_create_simple"
external get_simple_extent_dims : t -> int array * int array
  = "hdf5_h5s_get_simple_extent_dims"
external select_hyperslab : t -> Select.t -> start:Hsize.t array -> ?stride:Hsize.t array
  -> count:Hsize.t array -> ?block:Hsize.t array -> unit -> unit
  = "hdf5_h5s_select_hyperslab_bytecode" "hdf5_h5s_select_hyperslab"

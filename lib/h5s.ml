open H5

type t

module Class = struct
  type t =
  | NO_CLASS
  | SCALAR
  | SIMPLE
  | NULL
end

external close : t -> unit = "hdf5_h5s_close"
external create : Class.t -> t = "hdf5_h5s_create"
external create_simple : ?maximum_dims:int array -> current_dims:Hsize.t array -> unit
  -> t = "hdf5_h5s_create_simple"

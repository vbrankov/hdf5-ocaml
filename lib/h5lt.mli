open Bigarray

module Dataset_info : sig
  type t = {
    dims      : Hsize.t array;
    class_id  : H5t.Class.t;
    type_size : int;
  }
end

external make_dataset : Loc.t -> string -> Hsize.t array -> H5t.t -> _ Genarray.t -> unit
  = "hdf5_h5lt_make_dataset"
external read_dataset_int : Loc.t -> string -> _ Genarray.t -> unit
  = "hdf5_h5lt_read_dataset_int"
external get_dataset_info : Loc.t -> string -> Dataset_info.t
  = "hdf5_h5lt_get_dataset_info"
external set_attribute_int : Loc.t -> string -> string -> (int32, int32_elt, _) Array1.t
  -> unit = "hdf5_h5lt_set_attribute_int"
external get_attribute_int : Loc.t -> string -> string -> (int32, int32_elt, _) Array1.t
  -> unit = "hdf5_h5lt_get_attribute_int"

type t

module Cls_id = struct
  type t =
  | OBJECT_CREATE
  | FILE_CREATE
  | FILE_ACCESS
  | DATASET_CREATE
  | DATASET_ACCESS
  | DATASET_XFER
  | FILE_MOUNT
  | GROUP_CREATE
  | GROUP_ACCESS
  | DATATYPE_CREATE
  | DATATYPE_ACCESS
  | STRING_CREATE
  | ATTRIBUTE_CREATE
  | OBJECT_COPY
  | LINK_CREATE
  | LINK_ACCESS
end

external create : Cls_id.t -> t = "hdf5_h5p_create"
external close : t -> unit = "hdf5_h5p_close"
external set_userblock : t -> int -> unit = "hdf5_h5p_set_userblock"
external get_layout : t -> Layout.t = "hdf5_h5p_get_layout"
external set_chunk : t -> Hsize.t array -> unit = "hdf5_h5p_set_chunk"
external get_chunk : t -> Hsize.t array = "hdf5_h5p_get_chunk"
external set_deflate : t -> int -> unit = "hdf5_h5p_set_deflate"

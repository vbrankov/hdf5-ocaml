type t

module Cls_id : sig
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

module Alloc : sig
  type 'a t = int -> 'a
end

module Free : sig
  type 'a t = 'a -> unit
end

external create : Cls_id.t -> t = "hdf5_h5p_create"
external close : t -> unit = "hdf5_h5p_close"
external set_userblock : t -> int -> unit = "hdf5_h5p_set_userblock"
external set_create_intermediate_group : t -> bool -> unit
  = "hdf5_h5p_set_create_intermediate_group"
external get_create_intermediate_group : t -> bool
  = "hdf5_h5p_get_create_intermediate_group"
external get_layout : t -> Layout.t = "hdf5_h5p_get_layout"
external set_chunk : t -> Hsize.t array -> unit = "hdf5_h5p_set_chunk"
external get_chunk : t -> Hsize.t array = "hdf5_h5p_get_chunk"
external set_deflate : t -> int -> unit = "hdf5_h5p_set_deflate"
external set_filter : t -> H5z.Filter.t -> H5z.Flag.t list -> int array -> unit
  = "hdf5_h5p_set_filter"
external set_fletcher32 : t -> unit = "hdf5_h5p_set_fletcher32"
external set_nbit : t -> unit = "hdf5_h5p_set_nbit"
external set_shuffle : t -> unit = "hdf5_h5p_set_shuffle"
external set_vlen_mem_manager : t -> 'a Alloc.t -> 'a Free.t -> unit
  = "hdf5_h5p_set_vlen_mem_manager"
external get_vlen_mem_manager : t -> ('a Alloc.t * 'a Free.t)
  = "hdf5_h5p_set_vlen_mem_manager"

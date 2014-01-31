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

external close : t -> int = "caml_h5p_close"
external create : Cls_id.t -> t = "caml_h5p_create"
external set_userblock : t -> int -> unit = "caml_h5p_set_userblock"

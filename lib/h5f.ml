type t

module Acc = struct
  type t =
  | RDONLY
  | RDWR
  | TRUNC
  | EXCL
  | DEBUG
  | CREAT
  | DEFAULT
end

module Obj = struct
  type t =
  | FILE
  | DATASET
  | GROUP
  | DATATYPE
  | ATTR
  | ALL
  | LOCAL
end

module Scope = struct
  type t =
  | LOCAL
  | GLOBAL
end

module Close_degree = struct
  type t =
  | DEFAULT
  | WEAK
  | SEMI
  | STRONG
end

module Info = struct
  module Sohm = struct
    type t = {
      hdr_size  : Hsize.t;
      msgs_info : H5.Ih_info.t
    }
  end

  type t = {
    super_ext_size : Hsize.t;
    sohm           : Sohm.t;
  }
end

module Mem = struct
  type t =
  | DEFAULT
  | SUPER
  | BTREE
  | DRAW
  | GHEAP
  | LHEAP
  | OHDR
  | NTYPES
end

module Libver = struct
  type t =
  | EARLIEST
  | LATEST
end

external to_loc : t -> Loc.t = "%identity"
external of_loc : Loc.t -> t = "%identity"

external create : string -> ?fcpl:H5p.t -> ?fapl:H5p.t -> Acc.t list -> t
  = "hdf5_h5f_create"
external open_ : string -> ?fapl:H5p.t -> Acc.t list -> t = "hdf5_h5f_open"
external close : t -> unit = "hdf5_h5f_close"
external flush : Loc.t -> Scope.t -> unit = "hdf5_h5f_flush"
external get_obj_count : t -> Obj.t list -> int = "hdf5_h5f_get_obj_count"
external get_obj_ids : t -> Obj.t list -> Loc.t array = "hdf5_h5f_get_obj_ids"

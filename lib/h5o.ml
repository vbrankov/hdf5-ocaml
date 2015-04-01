module Type = struct
  type t =
  | GROUP
  | DATASET
  | NAMED_DATATYPE
  | NTYPES
end

module Hdr_info = struct
  module Space = struct
    type t = {
      total : int;
      meta  : int;
      mesg  : int;
      free  : int }
  end

  module Mesg = struct
    type t = {
      present : int;
      shared  : int }
  end

  type t = {
    version : int;
    nmesgs  : int;
    nchunks : int;
    flags   : int;
    space   : Space.t;
    mesg    : Mesg.t }
end

module Info = struct
  module Meta_size = struct
    type t = {
      obj : H5.Ih_info.t;
      attr : H5.Ih_info.t }
  end

  type t = {
    fileno    : int;
    addr      : int64;
    type_     : Type.t;
    rc        : int;
    atime     : H5.Time.t;
    mtime     : H5.Time.t;
    ctime     : H5.Time.t;
    btime     : H5.Time.t;
    num_attrs : int;
    hdr       : Hdr_info.t;
    meta_size : Meta_size.t }
end

module Msg_crt_idx = struct
  type t = int
end

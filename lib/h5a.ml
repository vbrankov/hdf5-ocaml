type t

module Info = struct
  type t = {
    corder_valid : bool;
    corder       : H5o.Msg_crt_idx.t;
    cset         : H5t.Cset.t;
    data_size    : int }
end

module Iterate = struct
  type h5a = t
  type 'a t = Loc.t -> string -> Info.t -> 'a -> H5.Iter.t
end

external create : Loc.t -> string -> H5t.t -> ?acpl:H5p.t -> ?aapl:H5p.t -> H5s.t -> t
  = "hdf5_h5a_create_bytecode" "hdf5_h5a_create"
external open_name : Loc.t -> string -> t = "hdf5_h5a_open_name"
external open_idx : Loc.t -> int -> t = "hdf5_h5a_open_idx"
external write : t -> H5t.t -> _ -> unit = "hdf5_h5a_write"
external read : t -> H5t.t -> _ -> unit = "hdf5_h5a_read"
external read_vl : t -> H5t.t -> string array -> unit = "hdf5_h5a_read_vl"
external close : t -> unit = "hdf5_h5a_close"
external iterate : Loc.t -> ?idx_type:H5.Index.t -> ?iter_order:H5.Iter_order.t
  -> ?n:int ref -> 'a Iterate.t -> 'a -> unit
  = "hdf5_h5a_iterate_bytecode" "hdf5_h5a_iterate"
external get_space : t -> H5s.t = "hdf5_h5a_get_space"
external get_type : t -> H5t.t = "hdf5_h5a_get_type"

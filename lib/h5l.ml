type t

module Type = struct
  type t =
  | HARD
  | SOFT
  | EXTERNAL
  | MAX
end

module Info = struct
  type t = {
    type_ : Type.t;
    corder_valid : bool;
    corder : int;
    cset : H5t.Cset.t;
    address : Loc.t option;
  }
end

module Iterate = struct
  type 'a t = Loc.t -> string -> Info.t -> 'a -> H5.Iter.t
end

external create_hard : Loc.t -> string -> Loc.t -> ?lcpl:H5p.t -> ?lapl:H5p.t -> string
  -> t = "hdf5_h5l_create_hard_bytecode" "hdf5_h5l_create_hard"
external exists : Loc.t -> ?lapl:H5p.t -> string -> bool = "hdf5_h5l_exists"
external move : Loc.t -> string -> Loc.t -> ?lcpl:H5p.t -> ?lapl:H5p.t -> string -> unit
  = "hdf5_h5l_move_bytecode" "hdf5_h5l_move"
external copy : Loc.t -> string -> Loc.t -> ?lcpl:H5p.t -> ?lapl:H5p.t -> string -> unit
  = "hdf5_h5l_copy_bytecode" "hdf5_h5l_copy"
external delete : Loc.t -> ?lapl:H5p.t -> string -> unit = "hdf5_h5l_delete"
external iterate : Loc.t -> H5.Index.t -> H5.Iter_order.t -> ?idx:int ref -> 'a Iterate.t
  -> 'a -> H5.Iter.t = "hdf5_h5l_iterate_bytecode" "hdf5_h5l_iterate"
external iterate_by_name : Loc.t -> string -> H5.Index.t -> H5.Iter_order.t
  -> ?idx:int ref -> 'a Iterate.t -> ?lapl:H5p.t -> 'a -> H5.Iter.t
  = "hdf5_h5l_iterate_by_name_bytecode" "hdf5_h5l_iterate_by_name"
external get_name_by_idx : Loc.t -> string -> H5.Index.t -> H5.Iter_order.t -> ?lapl:H5p.t
  -> int -> string = "hdf5_h5l_get_name_by_idx_bytecode" "hdf5_h5l_get_name_by_idx"

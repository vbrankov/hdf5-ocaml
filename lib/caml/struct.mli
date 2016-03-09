module Mem : sig
  type t = {
    ops      : int;
    data     : int;
    num_dims : int;
    flags    : int;
    proxy    : int;
    dim      : int;
  }
end

module Ptr : sig
  type t = {
    mutable ptr    : int;
    mutable mem    : Mem.t;
    mutable begin_ : int;
    mutable end_   : int;
    mutable len    : int;
    mutable i      : int;
  }

  val unsafe_next : t -> int -> unit
  val unsafe_prev : t -> int -> unit
  val unsafe_move : t -> int -> int -> unit
  val next : t -> int -> unit
  val prev : t -> int -> unit
  val move : t -> int -> int -> unit

  val get_float64  : t -> int -> float
  val set_float64  : t -> int -> float -> unit
  val get_int      : t -> int -> int
  val set_int      : t -> int -> int -> unit
  val get_int64    : t -> int -> int64
  val set_int64    : t -> int -> int64 -> unit
  val get_string   : t -> int -> int -> string
  val set_string   : t -> int -> int -> string -> unit

  val seek_int     : t -> int -> int -> int -> unit
  val seek_int64   : t -> int -> int -> int64 -> unit
  val seek_float64 : t -> int -> int -> float -> unit
  val seek_string  : t -> int -> int -> int -> string -> unit
end

module type S = sig
  val fields : Field.t list
end

module Make(S : S) : Struct_intf.S

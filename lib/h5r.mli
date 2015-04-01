open Bigarray

module Hobj_ref : sig
  type t
  val create : Loc.t -> ?space:H5s.t -> string -> t
  val dereference : Loc.t -> t -> Loc.t
  val get_region : Loc.t -> t -> H5s.t
  module Bigarray : sig
    type hobj_ref = t
    type t
    val create : int -> t
    val unsafe_get : t -> int -> hobj_ref
    val unsafe_set : t -> int -> hobj_ref -> unit
    val to_genarray : t -> (char, int8_unsigned_elt, c_layout) Genarray.t
  end
end

module Hdset_reg_ref : sig
  type t
  val create : Loc.t -> ?space:H5s.t -> string -> t
  val dereference : Loc.t -> t -> Loc.t
  val get_region : Loc.t -> t -> H5s.t
  module Bigarray : sig
    type hdset_reg_ref = t
    type t
    val create : int -> t
    val unsafe_get : t -> int -> hdset_reg_ref
    val unsafe_set : t -> int -> hdset_reg_ref -> unit
    val to_genarray : t -> (char, int8_unsigned_elt, c_layout) Genarray.t
  end
end


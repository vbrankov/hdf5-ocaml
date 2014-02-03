type t

module Loc : sig
  type t =
  [ `File of H5f.t
  | `Group of t ]
end

val close : t -> int
val create : Loc.t -> ?lcpl:H5p.t -> ?gcpl:H5p.t -> ?gapl:H5p.t -> string -> t
val open_ : Loc.t -> ?gapl:H5p.t -> string -> t

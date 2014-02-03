type t

module Loc = struct
  type t =
  [ `File of H5f.t
  | `Group of t ]
end

external close : t -> int = "caml_h5p_close"
external create : Loc.t -> ?lcpl:H5p.t -> ?gcpl:H5p.t -> ?gapl:H5p.t -> string -> t
  = "caml_h5g_create"
external open_ : Loc.t -> ?gapl:H5p.t -> string -> t = "caml_h5g_open"

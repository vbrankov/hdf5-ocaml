type t

module Acc : sig
  type t =
  | RDONLY
  | RDWR
  | TRUNC
  | EXCL
  | DEBUG
  | CREAT
  | DEFAULT
end

external to_loc : t -> Loc.t = "%identity"
external of_loc : Loc.t -> t = "%identity"

external close : t -> unit = "hdf5_h5f_close"
external create : string -> ?fcpl:H5p.t -> ?fapl:H5p.t -> Acc.t list -> t
  = "hdf5_h5f_create"
external open_ : string -> ?fapl:H5p.t -> Acc.t list -> t = "hdf5_h5f_open"

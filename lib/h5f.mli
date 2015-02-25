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

external close : t -> unit = "caml_h5f_close"
external create : string -> ?fcpl_id:H5p.t -> ?fapl_id:H5p.t -> Acc.t list -> t
  = "caml_h5f_create"
external open_ : string -> ?fapl_id:H5p.t -> Acc.t list -> t = "caml_h5f_open"

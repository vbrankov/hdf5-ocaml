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

external close : t -> int = "caml_h5f_close"
external create : string -> ?fcpl_id:H5p.t -> ?fapl_id:H5p.t -> Acc.t list -> t
  = "caml_h5f_create"

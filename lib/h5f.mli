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

val close : t -> int
val create : string -> ?fcpl_id:H5p.t -> ?fapl_id:H5p.t -> Acc.t list -> t
val open_ : string -> ?fapl_id:H5p.t -> Acc.t list -> t

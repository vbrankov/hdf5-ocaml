type t

module Class : sig
  type t =
  | NO_CLASS
  | SCALAR
  | SIMPLE
  | NULL
end

val close : t -> int
val create : Class.t -> t

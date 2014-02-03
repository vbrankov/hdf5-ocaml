type t

module Class = struct
  type t =
  | NO_CLASS
  | SCALAR
  | SIMPLE
  | NULL
end

external close : t -> int = "caml_h5s_close"
external create : Class.t -> t = "caml_h5s_create"

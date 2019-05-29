type t = private {
  name  : string;
  type_ : Type.t;
}

val create : string -> Type.t -> t

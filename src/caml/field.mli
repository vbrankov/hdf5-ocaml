type t = private {
  name  : string;
  type_ : Type.t;
}

val create : string -> _ Type.Unpacked.t -> t

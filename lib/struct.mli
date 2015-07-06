module Type : sig
  type t =
  | Int
  | Int64
  | Float64
  | String of int
end

module Field : sig
  type t = private {
    name  : string;
    type_ : Type.t;
  }

  val create : string -> Type.t -> t
end

module type S = sig
  val fields : Field.t list
end

module Make(S : S) : sig
  val fields : Field.t list
  val nfields : int
  val size : int
  val field_names : string array
  val field_offset : int array
  val field_types : Hid.t array
  val field_sizes : int array
  
  type t

  val create : unit -> t

  val unsafe_next : t -> unit
  val unsafe_move : t -> int -> unit

  val get_string : t -> int -> int -> string
  val set_string : t -> int -> int -> string -> unit
  
  val get_int_0 : t -> int
  val get_int_1 : t -> int
  val get_int_2 : t -> int
  val get_int_3 : t -> int
  val get_int_4 : t -> int
  val get_int_5 : t -> int
  val get_int_6 : t -> int
  val get_int_7 : t -> int
  val set_int_0 : t -> int -> unit
  val set_int_1 : t -> int -> unit
  val set_int_2 : t -> int -> unit
  val set_int_3 : t -> int -> unit
  val set_int_4 : t -> int -> unit
  val set_int_5 : t -> int -> unit
  val set_int_6 : t -> int -> unit
  val set_int_7 : t -> int -> unit

  val get_int64_0 : t -> int64
  val get_int64_1 : t -> int64
  val get_int64_2 : t -> int64
  val get_int64_3 : t -> int64
  val get_int64_4 : t -> int64
  val get_int64_5 : t -> int64
  val get_int64_6 : t -> int64
  val get_int64_7 : t -> int64
  val set_int64_0 : t -> int64 -> unit
  val set_int64_1 : t -> int64 -> unit
  val set_int64_2 : t -> int64 -> unit
  val set_int64_3 : t -> int64 -> unit
  val set_int64_4 : t -> int64 -> unit
  val set_int64_5 : t -> int64 -> unit
  val set_int64_6 : t -> int64 -> unit
  val set_int64_7 : t -> int64 -> unit

  val get_float64_0 : t -> float
  val get_float64_1 : t -> float
  val get_float64_2 : t -> float
  val get_float64_3 : t -> float
  val get_float64_4 : t -> float
  val get_float64_5 : t -> float
  val get_float64_6 : t -> float
  val get_float64_7 : t -> float
  val set_float64_0 : t -> float -> unit
  val set_float64_1 : t -> float -> unit
  val set_float64_2 : t -> float -> unit
  val set_float64_3 : t -> float -> unit
  val set_float64_4 : t -> float -> unit
  val set_float64_5 : t -> float -> unit
  val set_float64_6 : t -> float -> unit
  val set_float64_7 : t -> float -> unit

  module Array : sig
    type e = t
    type t

    val create : int -> t
    val unsafe_get : t -> int -> e
    val make_table : t -> title:string -> Hid.t -> string -> chunk_size:int
      -> compress:bool -> unit
    val read_table : Hid.t -> string -> t
  end

  val mem : t -> Array.t

  module Vector : sig
    type e = t
    type t

    val create : ?capacity:int -> unit -> t
    val resize : t -> int -> unit
    val append : t -> e
    val unsafe_get : t -> int -> e
    val iter : t -> f:(e -> unit) -> unit
    val to_array : t -> Array.t
  end
end

module type S = sig
  val fields : Field.t list
  val nfields : int
  val size : int
  val field_names : string array
  val field_offset : int array
  val field_sizes : int array

  (* [t = Ptr.t] is hidden so that the users cannot access low level functions. *)
  type t

  val create : unit -> t

  val pos : t -> int
  val has_next : t -> bool
  val has_prev : t -> bool

  module Array : sig
    type e = t
    type t

    val make : int -> t
    val init : int -> (int -> e -> unit) -> t
    val length : t -> int
    val unsafe_get : t -> int -> e
    val get : t -> int -> e
    val iter : t -> f:(e -> unit) -> unit
    val iteri : t -> f:(int -> e -> unit) -> unit

    (** Creates and writes a table. *)
    val make_table : t -> ?title:string -> ?chunk_size:int -> ?compress:bool -> H5caml.t
      -> string -> unit

    (** Adds records to the end of the table. *)
    val append_records : t -> H5caml.t -> string -> unit

    (** Ovewrites records. *)
    val write_records : t -> H5caml.t -> start:int -> string -> unit

    (** Reads a table. *)
    val read_table : H5caml.t -> string -> t

    (** Reads records. *)
    val read_records : H5caml.t -> start:int -> nrecords:int -> string -> t
  end

  val mem : t -> Array.t

  module Vector : sig
    type e = t
    type t

    val create : ?capacity:int -> ?growth_factor:float -> unit -> t
    val length : t -> int
    val realloc : t -> int -> unit
    val append : t -> e
    val clear : t -> unit
    val unsafe_get : t -> int -> e
    val get : t -> int -> e
    val iter : t -> f:(e -> unit) -> unit
    val of_array : ?growth_factor:float -> Array.t -> t
    val to_array : t -> Array.t
    val on_realloc : t -> (unit -> unit) -> unit
  end
end

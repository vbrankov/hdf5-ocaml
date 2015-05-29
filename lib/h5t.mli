type t

module Class : sig
  type t =
  | NO_CLASS
  | INTEGER
  | FLOAT
  | TIME
  | STRING
  | BITFIELD
  | OPAQUE
  | COMPOUND
  | REFERENCE
  | ENUM
  | VLEN
  | ARRAY
  | NCLASSES
end

module Order : sig
  type t =
  | ERROR
  | LE
  | BE
  | VAX
  | NONE
end

module Sign : sig
  type t =
  | NONE
  | SIGN_2
  | NSGN
end

module Norm : sig
  type t =
  | IMPLIED
  | MSBSET
  | NONE
end

module Cset : sig
  type t =
  | ASCII
  | UTF8
end

module Str : sig
  type t =
  | NULLTERM
  | NULLPAD
  | SPACEPAD
end

module Pad : sig
  type t =
  | ZERO
  | ONE
  | BACKGROUND
  | NPAD
end

module Cmd : sig
  type t =
  | INIT
  | CONV
  | FREE
end

module Bkg : sig
  type t =
  | NO
  | TEMP
  | YES
end

module Cdata : sig
  type t = {
    command  : Cmd.t;
    need_bkg : Bkg.t;
    recalc   : bool;
    priv     : int64 }
end

module Pers : sig
  type t =
  | DONTCARE
  | HARD
  | SOFT
end

module Direction : sig
  type t =
  | DEFAULT
  | ASCEND
  | DESCEND
end

module Conv_except : sig
  type t =
  | RANGE_HI
  | RANGE_LOW
  | PRECISION
  | TRUNCATE
  | PINF
  | NINF
  | NAN
end

module Conv_ret : sig
  type t =
  | ABORT
  | UNHANDLED
  | HANDLED
end

val variable : int

val ieee_f32be : t
val ieee_f32le : t
val ieee_f64be : t
val ieee_f64le : t
val std_i8be : t
val std_i8le : t
val std_i16be : t
val std_i16le : t
val std_i32be : t
val std_i32le : t
val std_i64be : t
val std_i64le : t
val std_u8be : t
val std_u8le : t
val std_u16be : t
val std_u16le : t
val std_u32be : t
val std_u32le : t
val std_u64be : t
val std_u64le : t
val std_b8be : t
val std_b8le : t
val std_b16be : t
val std_b16le : t
val std_b32be : t
val std_b32le : t
val std_b64be : t
val std_b64le : t
val std_ref_obj : t
val std_ref_dsetreg : t
val unix_d32be : t
val unix_d32le : t
val unix_d64be : t
val unix_d64le : t
val c_s1 : t
val fortran_s1 : t
val intel_i8 : t
val intel_i16 : t
val intel_i32 : t
val intel_i64 : t
val intel_u8 : t
val intel_u16 : t
val intel_u32 : t
val intel_u64 : t
val intel_b8 : t
val intel_b16 : t
val intel_b32 : t
val intel_b64 : t
val intel_f32 : t
val intel_f64 : t
val alpha_i8 : t
val alpha_i16 : t
val alpha_i32 : t
val alpha_i64 : t
val alpha_u8 : t
val alpha_u16 : t
val alpha_u32 : t
val alpha_u64 : t
val alpha_b8 : t
val alpha_b16 : t
val alpha_b32 : t
val alpha_b64 : t
val alpha_f32 : t
val alpha_f64 : t
val mips_i8 : t
val mips_i16 : t
val mips_i32 : t
val mips_i64 : t
val mips_u8 : t
val mips_u16 : t
val mips_u32 : t
val mips_u64 : t
val mips_b8 : t
val mips_b16 : t
val mips_b32 : t
val mips_b64 : t
val mips_f32 : t
val mips_f64 : t
val vax_f32 : t
val vax_f64 : t
val native_char : t
val native_schar : t
val native_uchar : t
val native_short : t
val native_ushort : t
val native_int : t
val native_uint : t
val native_long : t
val native_ulong : t
val native_llong : t
val native_ullong : t
val native_float : t
val native_double : t
val native_ldouble : t
val native_b8 : t
val native_b16 : t
val native_b32 : t
val native_b64 : t
val native_opaque : t
val native_haddr : t
val native_hsize : t
val native_hssize : t
val native_herr : t
val native_hbool : t
val native_int8 : t
val native_uint8 : t
val native_int_least8 : t
val native_uint_least8 : t
val native_int_fast8  : t
val native_uint_fast8 : t
val native_int16 : t
val native_uint16 : t
val native_int_least16 : t
val native_uint_least16 : t
val native_int_fast16 : t
val native_uint_fast16 : t
val native_int32 : t
val native_uint32 : t
val native_int_least32 : t
val native_uint_least32 : t
val native_int_fast32 : t
val native_uint_fast32 : t
val native_int64 : t
val native_uint64 : t
val native_int_least64 : t
val native_uint_least64  : t
val native_int_fast64 : t
val native_uint_fast64 : t

external to_loc : t -> Loc.t = "%identity"
external of_loc : Loc.t -> t = "%identity"

external create : Class.t -> int -> t = "hdf5_h5t_create"
external commit : Loc.t -> string -> ?lcpl:H5p.t -> ?tcpl:H5p.t -> ?tapl:H5p.t -> t
  -> unit = "hdf5_h5t_commit_bytecode" "hdf5_h5t_commit"
external copy : t -> t = "hdf5_h5t_copy"
external equal : t -> t -> bool = "hdf5_h5t_equal"
external get_class : t -> Class.t = "hdf5_h5t_get_class"
external set_size : t -> int -> unit = "hdf5_h5t_set_size"
external get_size : t -> int = "hdf5_h5t_get_size"
external get_native_type : t -> Direction.t -> t = "hdf5_h5t_get_native_type"
external close : t -> unit = "hdf5_h5t_close"
external get_order : t -> Order.t = "hdf5_h5t_get_order"
external set_order : t -> Order.t -> unit = "hdf5_h5t_set_order"
external get_strpad : t -> Str.t = "hdf5_h5t_get_strpad"
external set_strpad : t -> Str.t -> unit = "hdf5_h5t_set_strpad"
external is_variable_str : t -> bool = "hdf5_h5t_is_variable_str"
external get_nmembers : t -> int = "hdf5_h5t_get_nmembers"
external insert : t -> string -> int -> t -> unit = "hdf5_h5t_insert"

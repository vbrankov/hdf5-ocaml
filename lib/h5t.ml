type t

module Class = struct
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

module Order = struct
  type t =
  | ERROR
  | LE
  | BE
  | VAX
  | NONE
end

external datatypes : unit -> (t * t * t * t * t * t * t * t * t * t * t * t * t * t * t *
t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t
* t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t *
t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t
* t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t *
t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t)
= "hdf5_h5t_datatypes"

let ieee_f32be, ieee_f32le, ieee_f64be, ieee_f64le, std_i8be, std_i8le, std_i16be,
  std_i16le, std_i32be, std_i32le, std_i64be, std_i64le, std_u8be, std_u8le, std_u16be,
  std_u16le, std_u32be, std_u32le, std_u64be, std_u64le, std_b8be, std_b8le, std_b16be,
  std_b16le, std_b32be, std_b32le, std_b64be, std_b64le, std_ref_obj, std_ref_dsetreg,
  unix_d32be, unix_d32le, unix_d64be, unix_d64le, c_s1, fortran_s1, intel_i8, intel_i16,
  intel_i32, intel_i64, intel_u8, intel_u16, intel_u32, intel_u64, intel_b8, intel_b16,
  intel_b32, intel_b64, intel_f32, intel_f64, alpha_i8, alpha_i16, alpha_i32, alpha_i64,
  alpha_u8, alpha_u16, alpha_u32, alpha_u64, alpha_b8, alpha_b16, alpha_b32, alpha_b64,
  alpha_f32, alpha_f64, mips_i8, mips_i16, mips_i32, mips_i64, mips_u8, mips_u16,
  mips_u32, mips_u64, mips_b8, mips_b16, mips_b32, mips_b64, mips_f32, mips_f64, vax_f32,
  vax_f64, native_char, native_schar, native_uchar, native_short, native_ushort,
  native_int, native_uint, native_long, native_ulong, native_llong, native_ullong,
  native_float, native_double, native_ldouble, native_b8, native_b16, native_b32,
  native_b64, native_opaque, native_haddr, native_hsize, native_hssize, native_herr,
  native_hbool, native_int8, native_uint8, native_int_least8, native_uint_least8,
  native_int_fast8 , native_uint_fast8, native_int16, native_uint16, native_int_least16,
  native_uint_least16, native_int_fast16, native_uint_fast16, native_int32, native_uint32,
  native_int_least32, native_uint_least32, native_int_fast32, native_uint_fast32,
  native_int64, native_uint64, native_int_least64, native_uint_least64 ,
  native_int_fast64, native_uint_fast64 = datatypes ()

external create : Class.t -> int -> t = "hdf5_h5t_create"
external copy : t -> t = "hdf5_h5t_copy"
external get_class : t -> Class.t = "hdf5_h5t_get_class"
external get_size : t -> int = "hdf5_h5t_get_size"
external close : t -> unit = "hdf5_h5t_close"
external get_order : t -> Order.t = "hdf5_h5t_get_order"
external set_order : t -> Order.t -> unit = "hdf5_h5t_set_order"
external insert : t -> string -> int -> t -> unit = "hdf5_h5t_insert"

open Bigarray

module Dataset_info : sig
  type t = {
    dims      : Hsize.t array;
    class_id  : H5t.Class.t;
    type_size : int;
  }
end

external make_dataset                 : Loc.t -> string -> H5t.t -> _ Genarray.t -> unit = "hdf5_h5lt_make_dataset"
external make_dataset_int8_signed     : Loc.t -> string -> (int      , int8_signed_elt   , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_char"
external make_dataset_int8_unsigned   : Loc.t -> string -> (int      , int8_unsigned_elt , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_char"
external make_dataset_int16_signed    : Loc.t -> string -> (int      , int16_signed_elt  , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_short"
external make_dataset_int16_unsigned  : Loc.t -> string -> (int      , int16_unsigned_elt, _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_short"
external make_dataset_int32           : Loc.t -> string -> (int32    , int32_elt         , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_int"
external make_dataset_int64           : Loc.t -> string -> (int64    , int64_elt         , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_long"
external make_dataset_int             : Loc.t -> string -> (int      , int_elt           , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_long"
external make_dataset_nativeint       : Loc.t -> string -> (nativeint, nativeint_elt     , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_long"
external make_dataset_float           : Loc.t -> string -> (float    , float32_elt       , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_float"
external make_dataset_double          : Loc.t -> string -> (float    , float64_elt       , _) Genarray.t -> unit = "hdf5_h5lt_make_dataset_double"
external read_dataset_int8_signed     : Loc.t -> string -> (int      , int8_signed_elt   , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_char"
external read_dataset_int8_unsigned   : Loc.t -> string -> (int      , int8_unsigned_elt , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_char"
external read_dataset_int16_signed    : Loc.t -> string -> (int      , int16_signed_elt  , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_short"
external read_dataset_int16_unsigned  : Loc.t -> string -> (int      , int16_unsigned_elt, _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_short"
external read_dataset_int32           : Loc.t -> string -> (int32    , int32_elt         , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_int"
external read_dataset_int64           : Loc.t -> string -> (int64    , int64_elt         , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_long"
external read_dataset_int             : Loc.t -> string -> (int      , int_elt           , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_long"
external read_dataset_nativeint       : Loc.t -> string -> (nativeint, nativeint_elt     , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_long"
external read_dataset_float32         : Loc.t -> string -> (float    , float32_elt       , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_float"
external read_dataset_float64         : Loc.t -> string -> (float    , float64_elt       , _) Genarray.t -> unit = "hdf5_h5lt_read_dataset_double"
external get_dataset_info             : Loc.t -> string -> Dataset_info.t = "hdf5_h5lt_get_dataset_info"
external set_attribute_int8_signed    : Loc.t -> string -> string -> (int       , int8_signed_elt    , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_char"
external set_attribute_int8_unsigned  : Loc.t -> string -> string -> (int       , int8_unsigned_elt  , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_char"
external set_attribute_int16_signed   : Loc.t -> string -> string -> (int       , int16_signed_elt   , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_short"
external set_attribute_int16_unsigned : Loc.t -> string -> string -> (int       , int16_unsigned_elt , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_short"
external set_attribute_int32          : Loc.t -> string -> string -> (int32     , int32_elt          , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_int"
external set_attribute_int64          : Loc.t -> string -> string -> (int64     , int64_elt          , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_long"
external set_attribute_int            : Loc.t -> string -> string -> (int       , int_elt            , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_long"
external set_attribute_nativeint      : Loc.t -> string -> string -> (nativeint , nativeint_elt      , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_long"
external set_attribute_float32        : Loc.t -> string -> string -> (float     , float32_elt        , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_float"
external set_attribute_float64        : Loc.t -> string -> string -> (float     , float64_elt        , _) Array1.t -> unit = "hdf5_h5lt_set_attribute_double"
external get_attribute_int8_signed    : Loc.t -> string -> string -> (int       , int8_signed_elt    , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_char"
external get_attribute_int8_unsigned  : Loc.t -> string -> string -> (int       , int8_unsigned_elt  , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_char"
external get_attribute_int16_signed   : Loc.t -> string -> string -> (int       , int16_signed_elt   , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_short"
external get_attribute_int16_unsigned : Loc.t -> string -> string -> (int       , int16_unsigned_elt , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_short"
external get_attribute_int32          : Loc.t -> string -> string -> (int32     , int32_elt          , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_int"
external get_attribute_int64          : Loc.t -> string -> string -> (int64     , int64_elt          , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_long"
external get_attribute_int            : Loc.t -> string -> string -> (int       , int_elt            , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_long"
external get_attribute_nativeint      : Loc.t -> string -> string -> (nativeint , nativeint_elt      , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_long"
external get_attribute_float32        : Loc.t -> string -> string -> (float     , float32_elt        , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_float"
external get_attribute_float64        : Loc.t -> string -> string -> (float     , float64_elt        , _) Array1.t -> unit = "hdf5_h5lt_get_attribute_double"

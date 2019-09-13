open Bigarray

module Field_info : sig
  type t = {
    field_names   : string array;
    field_sizes   : int array;
    field_offsets : int array;
    type_size     : int;
  }
end

(** A pointer to the data. *)
module Data : sig
  type t

  val of_genarray : _ Genarray.t -> t
  val of_array1 : _ Array1.t -> t
  val of_array2 : _ Array2.t -> t
  val of_array3 : _ Array3.t -> t
end

external make_table : string -> Hid.t -> string -> nrecords:int -> type_size:int
  -> field_names:string array -> field_offset:int array -> field_types:Hid.t array
  -> chunk_size:int -> ?fill_data:Data.t -> compress:bool -> Data.t -> unit
  = "hdf5_h5tb_make_table_bytecode" "hdf5_h5tb_make_table"
external append_records : Hid.t -> string -> nrecords:int -> type_size:int
  -> field_offset:int array -> field_sizes:int array -> Data.t -> unit
  = "hdf5_h5tb_append_records_bytecode" "hdf5_h5tb_append_records"
external write_records : Hid.t -> string -> start:int -> nrecords:int -> type_size:int
  -> field_offset:int array -> field_sizes:int array -> Data.t -> unit
  = "hdf5_h5tb_write_records_bytecode" "hdf5_h5tb_write_records"
external read_fields_name : Hid.t -> string -> string -> start:int -> nrecords:int
  -> type_size:int -> field_offset:int array -> dst_sizes:int array -> Data.t -> unit
  = "hdf5_h5tb_read_fields_name_bytecode" "hdf5_h5tb_read_fields_name"
external read_table : Hid.t -> string -> dst_size:int -> dst_offset:int array
  -> dst_sizes:int array -> Data.t -> unit
  = "hdf5_h5tb_read_table_bytecode" "hdf5_h5tb_read_table"
external read_records : Hid.t -> string -> start:int -> nrecords:int -> type_size:int
  -> field_offset:int array -> dst_sizes:int array -> Data.t -> unit
  = "hdf5_h5tb_read_records_bytecode" "hdf5_h5tb_read_records"
external get_table_info : Hid.t -> string -> int = "hdf5_h5tb_get_table_info"
external get_field_info : Hid.t -> string -> Field_info.t = "hdf5_h5tb_get_field_info"

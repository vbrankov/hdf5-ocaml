external make_table : string -> Hid.t -> string -> nrecords:int -> type_size:int
  -> field_names:string array -> field_offset:int array -> field_types:Hid.t array
  -> chunk_size:int -> ?fill_data:_ -> compress:bool -> _ -> unit
  = "hdf5_h5tb_make_table_bytecode" "hdf5_h5tb_make_table"
external append_records : Hid.t -> string -> nrecords:int -> type_size:int
  -> field_offset:int array -> field_sizes:int array -> _ -> unit
  = "hdf5_h5tb_append_records_bytecords" "hdf5_h5tb_append_records"
external write_records : Hid.t -> string -> start:int -> nrecords:int -> type_size:int
  -> field_offset:int array -> field_sizes:int array -> _ -> unit
  = "hdf5_h5tb_write_records_bytecords" "hdf5_h5tb_write_records"
external read_table : Hid.t -> string -> dst_size:int -> dst_offset:int array
  -> dst_sizes:int array -> _ -> unit
  = "hdf5_h5tb_read_table_bytecode" "hdf5_h5tb_read_table"
external get_table_info : Hid.t -> string -> int = "hdf5_h5tb_get_table_info"

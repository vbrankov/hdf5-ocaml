external make_table : string -> Hid.t -> string -> nrecords:int -> type_size:int
  -> field_names:string array -> field_offset:int array -> field_types:Hid.t array
  -> chunk_size:int -> ?fill_data:_ -> compress:bool -> _ -> unit
  = "hdf5_h5tb_make_table_bytecode" "hdf5_h5tb_make_table"
external read_table : Hid.t -> string -> dst_size:int -> dst_offset:int array
  -> dst_sizes:int array -> _ -> unit
  = "hdf5_h5tb_read_table_bytecode" "hdf5_h5tb_read_table"

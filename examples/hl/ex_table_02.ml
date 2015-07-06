open Hdf5_caml

let _NRECORDS     = 8
let _NRECORDS_ADD = 2
let _TABLE_NAME   = "table"

module Particle = struct
  open Struct
  include Make(struct
    let fields = [
      Field.create "Name"        (Type.String 16);
      Field.create "Latitude"    Type.Int;
      Field.create "Longitude"   Type.Int;
      Field.create "Pressure"    Type.Float64;
      Field.create "Temperature" Type.Float64;
    ]
  end)

  let     name        t   = get_string t 0 16
  let set_name        t v = set_string t 0 16 v
  let     lati        t   = get_int_2 t
  let set_lati        t v = set_int_2 t v
  let     longi       t   = get_int_3 t
  let set_longi       t v = set_int_3 t v
  let     pressure    t   = get_float64_4 t
  let set_pressure    t v = set_float64_4 t v
  let     temperature t   = get_float64_5 t
  let set_temperature t v = set_float64_5 t v

  let set t name lati longi pressure temperature =
    set_name t name;
    set_lati t lati;
    set_longi t longi;
    set_pressure t pressure;
    set_temperature t temperature
end

let () =
  let dst_buf = Particle.Array.create (_NRECORDS + _NRECORDS_ADD) in
  let p_data = Particle.Vector.create () in
  Particle.(set (Vector.append p_data) "zero"   0  0 0.  0.);
  Particle.(set (Vector.append p_data) "one"   10 10 1. 10.);
  Particle.(set (Vector.append p_data) "two"   20 20 2. 20.);
  Particle.(set (Vector.append p_data) "three" 30 30 3. 30.);
  Particle.(set (Vector.append p_data) "four"  40 40 4. 40.);
  Particle.(set (Vector.append p_data) "five"  50 50 5. 50.);
  Particle.(set (Vector.append p_data) "six"   60 60 6. 60.);
  Particle.(set (Vector.append p_data) "seven" 70 70 7. 70.);
  let p_data = Particle.Vector.compact p_data in

  let particle_in = Particle.Vector.create () in
  Particle.(set (Vector.append particle_in) "eight" 80 80 8. 80.);
  Particle.(set (Vector.append particle_in) "ning"  90 90 9. 90.);
  let particle_in = Particle.Vector.compact particle_in in

  let string_type = H5t.copy H5t.c_s1 in
  H5t.set_size string_type 16;
  let file_id = H5f.create "ex_table_02.h5" H5f.Acc.([ TRUNC ]) in
  H5tb.make_table "Table Title" file_id _TABLE_NAME ~nrecords:_NRECORDS
    ~type_size:Particle.size
    ~field_names:[| "Name"; "Latitude"; "Longitude"; "Pressure"; "Temperature" |]
    ~field_offset:[| 0; 16; 24; 32; 40 |]
    ~field_types:[|
      string_type; H5t.native_long; H5t.native_long; H5t.native_double; H5t.native_double
    |]
    ~chunk_size:10
    ~compress:false
    p_data;
  H5tb.append_records file_id _TABLE_NAME ~nrecords:_NRECORDS_ADD ~type_size:Particle.size
    ~field_offset:[| 0; 16; 24; 32; 40 |]
    ~field_sizes:[| 16; 8; 8; 8; 8 |]
    particle_in;
  H5tb.read_table file_id _TABLE_NAME ~dst_size:Particle.size
    ~dst_offset:[| 0; 16; 24; 32; 40 |]
    ~dst_sizes:[| 16; 8; 8; 8; 8 |]
    dst_buf;
  let p = Particle.Array.unsafe_get dst_buf 0 in
  for i = 0 to _NRECORDS + _NRECORDS_ADD - 1 do
    Printf.printf "%-5s %-5d %-5d %-5f %-5f\n%!"
      (Particle.name p) (Particle.lati p) (Particle.longi p) (Particle.pressure p)
      (Particle.temperature p);
    Particle.unsafe_next p
  done;
  H5t.close string_type;
  H5f.close file_id

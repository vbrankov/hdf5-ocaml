open Hdf5_raw

let _NRECORDS   = 8
let _TABLE_NAME = "table"

module Particle = struct
  [%h5struct
    name        "Name"        (String 16);
    lati        "Latitude"    Int;
    longi       "Longitude"   Int;
    pressure    "Pressure"    Float64;
    temperature "Temperature" Float64]
end

let () =
  let dst_buf = Particle.Array.create _NRECORDS in
  let p_data = Particle.Vector.create () in
  Particle.(set (Vector.append p_data) "zero"   0  0 0.  0.);
  Particle.(set (Vector.append p_data) "one"   10 10 1. 10.);
  Particle.(set (Vector.append p_data) "two"   20 20 2. 20.);
  Particle.(set (Vector.append p_data) "three" 30 30 3. 30.);
  Particle.(set (Vector.append p_data) "four"  40 40 4. 40.);
  Particle.(set (Vector.append p_data) "five"  50 50 5. 50.);
  Particle.(set (Vector.append p_data) "six"   60 60 6. 60.);
  Particle.(set (Vector.append p_data) "seven" 70 70 7. 70.);
  let p_data = Particle.Vector.to_array p_data in

  let string_type = H5t.copy H5t.c_s1 in
  H5t.set_size string_type 16;
  let file_id = H5f.create "ex_table_01.h5" H5f.Acc.([ TRUNC ]) in
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
  H5tb.read_table file_id _TABLE_NAME ~dst_size:Particle.size
    ~dst_offset:[| 0; 16; 24; 32; 40 |]
    ~dst_sizes:[| 16; 8; 8; 8; 8 |]
    dst_buf;
  let p = Particle.Array.unsafe_get dst_buf 0 in
  for i = 0 to _NRECORDS - 1 do
    Printf.printf "%-5s %-5d %-5d %-5f %-5f\n%!"
      (Particle.name p) (Particle.lati p) (Particle.longi p) (Particle.pressure p)
      (Particle.temperature p);
    Particle.unsafe_next p
  done;
  H5t.close string_type;
  H5f.close file_id

open Hdf5_raw
open Hdf5_caml
open Bigarray

let _NRECORDS       = 8
let _NRECORDS_WRITE = 2
let _TABLE_NAME     = "table"

module Particle = struct
  [%%h5struct
    name        "Name"        (String 16);
    lati        "Latitude"    Int;
    longi       "Longitude"   Int;
    pressure    "Pressure"    Float64;
    temperature "Temperature" Float64]
end

let () =
  let dst_buf = Particle.Array.make _NRECORDS in
  let fill_data = Particle.create () in
  Particle.set fill_data ~name:"no_data" ~lati:(-1) ~longi:(-1) ~pressure:(-99.0) ~temperature:(-99.0);
  let fill_data = Particle.Array.data (Particle.mem fill_data) |> genarray_of_array1 in

  let particle_in = Particle.Vector.create () in
  Particle.(set (Vector.append particle_in) ~name:"zero"  ~lati: 0 ~longi: 0 ~pressure:0. ~temperature: 0.);
  Particle.(set (Vector.append particle_in) ~name:"one"   ~lati:10 ~longi:10 ~pressure:1. ~temperature:10.);
  let particle_in = Particle.Vector.to_array particle_in in

  let string_type = H5t.copy H5t.c_s1 in
  H5t.set_size string_type 16;
  let file_id = H5f.create "ex_table_03.h5" H5f.Acc.([ TRUNC ]) in
  H5tb.make_table "Table Title" file_id _TABLE_NAME ~nrecords:_NRECORDS
    ~type_size:Particle.size
    ~field_names:[| "Name"; "Latitude"; "Longitude"; "Pressure"; "Temperature" |]
    ~field_offset:[| 0; 16; 24; 32; 40 |]
    ~field_types:[|
      string_type; H5t.native_long; H5t.native_long; H5t.native_double; H5t.native_double
    |]
    ~chunk_size:10
    ~fill_data
    ~compress:false
    (genarray_of_array1 (Array1.create float64 c_layout 0));
  H5tb.write_records file_id _TABLE_NAME ~start:0 ~nrecords:_NRECORDS_WRITE
    ~type_size:Particle.size
    ~field_offset:[| 0; 16; 24; 32; 40 |]
    ~field_sizes:[| 16; 8; 8; 8; 8 |]
    (Particle.Array.data particle_in |> genarray_of_array1);
  H5tb.read_table file_id _TABLE_NAME ~dst_size:Particle.size
    ~dst_offset:[| 0; 16; 24; 32; 40 |]
    ~dst_sizes:[| 16; 8; 8; 8; 8 |]
    (Particle.Array.data dst_buf |> genarray_of_array1);
  let p = Particle.Array.unsafe_get dst_buf 0 in
  for _ = 0 to _NRECORDS - 1 do
    Printf.printf "%-5s %-5d %-5d %-5f %-5f\n%!"
      (Particle.name p) (Particle.lati p) (Particle.longi p) (Particle.pressure p)
      (Particle.temperature p);
    Particle.unsafe_next p
  done;
  H5t.close string_type;
  H5f.close file_id

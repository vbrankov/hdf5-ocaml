open Hdf5_caml

let () =
  let h5 = H5.create_trunc "test.h5" in
  H5.write_float_array h5 "a" [| 0.; 1.; 2.; 3. |];
  let a = H5.open_dataset h5 "a" in
  H5.write_attribute_float a "b" 4.;
  H5.close h5

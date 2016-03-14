open Hdf5_caml

let () =
  let h5 = H5.create_trunc "test.h5" in
  H5.write_float_array h5 "a" [| 0.; 1.; 2.; 3. |];
  H5.close h5

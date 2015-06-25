open Hdf5_caml

let _FILE = "test.h5"

let () =
  let file = H5f.create _FILE H5f.Acc.([ TRUNC ]) in
  assert (H5f.get_name file = _FILE)

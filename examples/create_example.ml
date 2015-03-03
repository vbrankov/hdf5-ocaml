open Bigarray
open Hdf5_caml

let file = "SDS.h5"
let datasetname = "IntArray"
let nx = 5
let ny = 6
let rank = 2

let () =
  let data = Array2.create nativeint c_layout nx ny in
  for j = 0 to nx - 1 do
    for i = 0 to ny - 1 do
      data.{j, i} <- Nativeint.of_int (i + j)
    done
  done;
  let file = H5f.create file [ H5f.Acc.TRUNC ] in
  let dataspace = H5s.create_simple
    ~current_dims:[| H5.Hsize.of_int nx; H5.Hsize.of_int ny |] () in
  let datatype = H5t.copy H5t.native_int in
  H5t.set_order datatype H5t.Order.LE;
  let dataset = H5d.create (H5f.to_loc file) datasetname datatype dataspace in
  H5d.write dataset H5t.native_int ~mem_space:H5s.all ~file_space:H5s.all
    (genarray_of_array2 data);
  H5t.close datatype;
  H5d.close dataset;
  H5s.close dataspace;
  H5f.close file

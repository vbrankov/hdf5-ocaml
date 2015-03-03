open Bigarray
open Hdf5_caml

let file = "SDS.h5"
let datasetname = "IntArray"
let nx_sub = 3
let ny_sub = 4
let nx = 7
let ny = 7
let nz = 3
let rank = 2
let rank_out = 3

let () =
  let data_out = Array3.create nativeint c_layout nx ny nz in
  for j = 0 to nx - 1 do
    for i = 0 to ny - 1 do
      for k = 0 to nz - 1 do
        data_out.{j, i, k} <- Nativeint.zero
      done
    done
  done;

  let file = H5f.open_ file [ H5f.Acc.RDONLY ] in
  let dataset = H5d.open_ (H5f.to_loc file) datasetname in
  let datatype = H5d.get_type dataset in
  let class_ = H5t.get_class datatype in
  if class_ = H5t.Class.INTEGER then Printf.printf "Data set has INTEGER type\n";
  let order = H5t.get_order datatype in
  if order = H5t.Order.LE then Printf.printf "Little endian order\n";
  let size = H5t.get_size datatype in
  Printf.printf "Data size is %d\n" size;
  let dataspace = H5d.get_space dataset in
  let status_n, _ = H5s.get_simple_extent_dims dataspace in
  Printf.printf "rank %d, dimensions %d x %d\n" (Array.length status_n) status_n.(0)
    status_n.(1);
  H5s.select_hyperslab dataspace H5s.Select.SET
    ~start:(H5.Hsize.of_int_array [| 1; 2 |])
    ~count:(H5.Hsize.of_int_array [| nx_sub; ny_sub |]) ();
  let memspace = H5s.create_simple
    ~current_dims:(H5.Hsize.of_int_array [| nx; ny; nz |]) () in
  H5s.select_hyperslab memspace H5s.Select.SET
    ~start:(H5.Hsize.of_int_array [| 3; 0; 0 |])
    ~count:(H5.Hsize.of_int_array [| nx_sub; ny_sub; 1 |]) ();
  H5d.read dataset H5t.native_int memspace dataspace (genarray_of_array3 data_out);
  for j = 0 to nx - 1 do
    for i = 0 to ny - 1 do
      Printf.printf "%nd " data_out.{j, i, 0}
    done;
    Printf.printf "\n"
  done;
  H5t.close datatype;
  H5d.close dataset;
  H5s.close dataspace;
  H5s.close memspace;
  H5f.close file

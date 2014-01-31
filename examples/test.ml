open Hdf5_caml

let () =
  let cls_id = H5p.create H5p.Cls_id.FILE_CREATE in
  H5p.set_userblock cls_id 1024;
  let status = H5p.close cls_id in
  Printf.printf "%d\n" status;
  let file_id = H5f.create "SampleFile.h5" [ H5f.Acc.EXCL ] in
  let status = H5f.close file_id in
  Printf.printf "%d\n" status;
  let file_id = H5f.open_ "SampleFile.h5" [ H5f.Acc.RDONLY ] in
  let status = H5f.close file_id in
  Printf.printf "%d\n" status;
  let datatype = H5t.copy H5t.native_int in
  let status = H5t.set_order datatype H5t.Order.LE in
  Printf.printf "%d\n" status

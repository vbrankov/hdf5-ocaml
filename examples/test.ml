open Hdf5_caml

let () =
  let cls_id = H5p.create H5p.Cls_id.FILE_CREATE in
  H5p.set_userblock cls_id 1024;
  H5p.close cls_id;
  let file_id = H5f.create "SampleFile.h5" [ H5f.Acc.EXCL ] in
  H5f.close file_id;
  let file_id = H5f.open_ "SampleFile.h5" [ H5f.Acc.RDONLY ] in
  H5f.close file_id;
  let datatype = H5t.copy H5t.native_int in
  H5t.set_order datatype H5t.Order.LE

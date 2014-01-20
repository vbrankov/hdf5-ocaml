open Hdf5_caml

let () =
  let cls_id = H5p.create H5p.Cls_id.FILE_CREATE in
  let status = H5p.close cls_id in
  Printf.printf "%d\n" status;
  let file_id = H5f.create "SampleFile.h5" [ H5f.Acc.EXCL ] in
  let status = H5f.close file_id in
  Printf.printf "%d\n" status;
  let file_id = H5f.open_ "SampleFile.h5" [ H5f.Acc.RDONLY ] in
  let status = H5f.close file_id in
  Printf.printf "%d\n" status

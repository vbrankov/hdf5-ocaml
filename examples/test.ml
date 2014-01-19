open Hdf5_caml

let () =
  let cls_id = H5p.create H5p.Cls_id.FILE_CREATE in
  let status = H5p.close cls_id in
  Printf.printf "%d\n" status

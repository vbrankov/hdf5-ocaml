#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include "hdf5.h"

value caml_h5p_create(value caml_cls_id)
{
  CAMLparam1(caml_cls_id);

  CAMLlocal1(caml_v);
  hid_t cls_id, v;

  switch (Int_val(caml_cls_id))
  {
    case  0: cls_id = H5P_OBJECT_CREATE; break;
    case  1: cls_id = H5P_FILE_CREATE; break;
    case  2: cls_id = H5P_FILE_ACCESS; break;
    case  3: cls_id = H5P_DATASET_CREATE; break;
    case  4: cls_id = H5P_DATASET_ACCESS; break;
    case  5: cls_id = H5P_DATASET_XFER; break;
    case  6: cls_id = H5P_FILE_MOUNT; break;
    case  7: cls_id = H5P_GROUP_CREATE; break;
    case  8: cls_id = H5P_GROUP_ACCESS; break;
    case  9: cls_id = H5P_DATATYPE_CREATE; break;
    case 10: cls_id = H5P_DATATYPE_ACCESS; break;
    case 11: cls_id = H5P_STRING_CREATE; break;
    case 12: cls_id = H5P_ATTRIBUTE_CREATE; break;
    case 13: cls_id = H5P_OBJECT_COPY; break;
    case 14: cls_id = H5P_LINK_CREATE; break;
    case 15: cls_id = H5P_LINK_ACCESS; break;
    default: caml_failwith("unrecognized cls_id");
  }

  v = H5Pcreate(cls_id);
  caml_v = caml_copy_nativeint((long) v);
  
  CAMLreturn(caml_v);
}

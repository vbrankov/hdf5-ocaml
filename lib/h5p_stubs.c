#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5i_stubs.h"
#include "h5p_stubs.h"

static struct custom_operations h5p_ops = {
  "hdf5.h5p",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_h5p(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&h5p_ops, sizeof(hid_t), 0, 1);
  H5P_val(v) = id;
  return v;
}

void caml_h5p_close(value cls_id_v)
{
  CAMLparam1(cls_id_v);
  raise_if_fail(H5Pclose(H5P_val(cls_id_v)));
  CAMLreturn0;
}

value caml_h5p_create(value cls_id_v)
{
  CAMLparam1(cls_id_v);

  hid_t cls_id;

  switch (Int_val(cls_id_v))
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

  CAMLreturn(alloc_h5p(H5Pcreate(cls_id)));
}

void caml_h5p_set_userblock(value plist_v, value size_v)
{
  CAMLparam2(plist_v, size_v);

  hid_t plist = H5P_val(plist_v);
  hsize_t size = Int_val(size_v);

  raise_if_fail(H5Pset_userblock(plist, size));
  CAMLreturn0;
}

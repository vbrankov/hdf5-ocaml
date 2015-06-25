#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5i_stubs.h"
#include "hid_stubs.h"

static struct custom_operations hid_ops = {
  "hdf5.hid",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value alloc_hid(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&hid_ops, sizeof(hid_t), 0, 1);
  Hid_val(v) = id;
  return v;
}

value val_hid_array(int length, hid_t *a)
{
  CAMLparam0();
  CAMLlocal1(a_v);
  int i;

  a_v = caml_alloc_tuple(length);
  for (i = 0; i < length; i++)
    Field(a_v, i) = alloc_hid(a[i]);
  CAMLreturn(a_v);
}

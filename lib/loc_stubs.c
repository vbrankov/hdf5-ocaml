#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5i_stubs.h"
#include "loc_stubs.h"

static struct custom_operations loc_ops = {
  "hdf5.loc",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value alloc_loc(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&loc_ops, sizeof(hid_t), 0, 1);
  Loc_val(v) = id;
  return v;
}

value val_loc_array(int length, hid_t *a)
{
  CAMLparam0();
  CAMLlocal1(a_v);
  int i;

  a_v = caml_alloc_tuple(length);
  for (i = 0; i < length; i++)
    Field(a_v, i) = alloc_loc(a[i]);
  CAMLreturn(a_v);
}

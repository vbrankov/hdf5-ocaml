#include <assert.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5i_stubs.h"
#include "h5p_stubs.h"

static struct custom_operations h5g_ops = {
  "hdf5.h5g",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define H5G_val(v) *((hid_t*) Data_custom_val(v))

static value alloc_h5g(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&h5g_ops, sizeof(hid_t), 0, 1);
  H5G_val(v) = id;
  return v;
}

static hid_t loc_val(value v)
{
  return H5G_val(Field(v, 1));
}

void caml_h5g_close(value group_v)
{
  CAMLparam1(group_v);

  hid_t group_id = H5G_val(group_v);
  raise_if_fail(H5Gclose(group_id));
  CAMLreturn0;
}

value caml_h5g_create(value loc_v, value lcpl_v, value gcpl_v, value gapl_v, value name_v)
{
  CAMLparam5(loc_v, lcpl_v, gcpl_v, gapl_v, name_v);

  hid_t loc_id = loc_val(loc_v);
  hid_t lcpl_id = H5P_opt_val(lcpl_v);
  hid_t gcpl_id = H5P_opt_val(gcpl_v);
  hid_t gapl_id = H5P_opt_val(gapl_v);
  const char* name = String_val(name_v);

  CAMLreturn(alloc_h5g(H5Gcreate2(loc_id, name, lcpl_id, gcpl_id, gapl_id)));
}

value caml_h5g_open(value loc_v, value gapl_v, value name_v)
{
  CAMLparam3(loc_v, gapl_v, name_v);

  hid_t loc_id = loc_val(loc_v);
  hid_t gapl_id = H5P_opt_val(gapl_v);
  const char* name = String_val(name_v);

  CAMLreturn(alloc_h5g(H5Gopen2(loc_id, name, gapl_id)));
}

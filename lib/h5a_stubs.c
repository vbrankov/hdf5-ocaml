#include <assert.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5a_stubs.h"
#include "h5d_stubs.h"
#include "h5i_stubs.h"
#include "h5g_stubs.h"
#include "h5p_stubs.h"
#include "h5t_stubs.h"
#include "loc_stubs.h"

static struct custom_operations h5a_ops = {
  "hdf5.h5a",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_h5a(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&h5a_ops, sizeof(hid_t), 0, 1);
  H5A_val(v) = id;
  return v;
}

value hdf5_h5a_create(value loc_id_v, value attr_name_v, value type_id_v, value acpl_id_v,
  value aapl_id_v, value space_id_v)
{
  CAMLparam5(loc_id_v, attr_name_v, type_id_v, acpl_id_v, aapl_id_v);
  CAMLxparam1(space_id_v);

  hid_t loc_id, type_id, space_id, acpl_id, aapl_id;
  const char *attr_name;

  loc_id = Loc_val(loc_id_v);
  attr_name = String_val(attr_name_v);
  type_id = H5T_val(type_id_v);
  acpl_id = H5P_opt_val(acpl_id_v);
  aapl_id = H5P_opt_val(aapl_id_v);
  space_id = H5D_val(space_id_v);

  CAMLreturn(alloc_h5a(H5Acreate2(
    loc_id, attr_name, type_id, space_id, acpl_id, aapl_id)));
}

value hdf5_h5a_create_bytecode(value *argv, int argn)
{
  assert(argn == 6);
  return hdf5_h5a_create(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

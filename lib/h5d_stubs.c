#include <assert.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5d_stubs.h"
#include "h5g_stubs.h"
#include "h5i_stubs.h"
#include "h5p_stubs.h"
#include "h5s_stubs.h"
#include "h5t_stubs.h"
#include "loc_stubs.h"

static struct custom_operations h5d_ops = {
  "hdf5.h5d",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_h5d(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&h5d_ops, sizeof(hid_t), 0, 1);
  H5D_val(v) = id;
  return v;
}

value hdf5_h5d_create(value loc_id_v, value name_v, value dtype_id_v, value lcpl_id_v,
    value dcpl_id_v, value dapl_id_v, value space_id_v)
{
  CAMLparam5(loc_id_v, name_v, dtype_id_v, lcpl_id_v, dcpl_id_v);
  CAMLxparam2(dapl_id_v, space_id_v);

  hid_t loc_id = Loc_val(loc_id_v);
  const char* name = String_val(name_v);
  hid_t dtype_id = H5T_val(dtype_id_v);
  hid_t lcpl_id = H5P_opt_val(lcpl_id_v);
  hid_t dcpl_id = H5P_opt_val(dcpl_id_v);
  hid_t dapl_id = H5P_opt_val(dapl_id_v);
  hid_t space_id = H5S_val(space_id_v);

  CAMLreturn(alloc_h5d(
    H5Dcreate2(loc_id, name, dtype_id, space_id, lcpl_id, dcpl_id, dapl_id)));
}

value hdf5_h5d_create_bytecode(value *argv, int argn)
{
  assert(argn == 7);
  return hdf5_h5d_create(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

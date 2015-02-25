#include <assert.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5_stubs.h"
#include "h5i_stubs.h"
#include "h5p_stubs.h"

static struct custom_operations h5s_ops = {
  "hdf5.h5s",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define H5S_val(v) *((hid_t*) Data_custom_val(v))

static value alloc_h5s(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&h5s_ops, sizeof(hid_t), 0, 1);
  H5S_val(v) = id;
  return v;
}

H5S_class_t H5S_class_val(value class)
{
  switch (Int_val(class))
  {
    case 0: return H5S_NO_CLASS;
    case 1: return H5S_SCALAR;
    case 2: return H5S_SIMPLE;
    case 3: return H5S_NULL;
    default: caml_failwith("unrecognized H5S_class_t");
  }
}

void caml_h5s_close(value space_v)
{
  CAMLparam1(space_v);

  raise_if_fail(H5Sclose(H5S_val(space_v)));
  CAMLreturn0;
}

value caml_h5s_create(value type_v)
{
  CAMLparam1(type_v);

  CAMLreturn(alloc_h5s(H5Screate(H5S_class_val(type_v))));
}

value caml_h5s_create_simple(value maximum_dims_v, value current_dims_v, value unit_v)
{
  CAMLparam3(maximum_dims_v, current_dims_v, unit_v);
  int rank, maximum_dims_length;
  hsize_t *current_dims, *maximum_dims;
  hid_t id;

  rank = hsize_t_array_val(current_dims_v, &current_dims);
  if (current_dims == NULL)
    caml_raise_out_of_memory();
  maximum_dims_length = hsize_t_array_opt_val(maximum_dims_v, &maximum_dims);
  if (maximum_dims != NULL && rank != maximum_dims_length)
  {
    free(current_dims);
    free(maximum_dims);
    caml_invalid_argument(
      "H5s.create_simple: current_dims and maximum_dims not of same length");
  }
  if (maximum_dims == NULL && maximum_dims_length != 0)
  {
    free(current_dims);
    caml_raise_out_of_memory();
  }
  id = H5Screate_simple(rank, current_dims, maximum_dims);
  free(current_dims);
  free(maximum_dims);

  CAMLreturn(alloc_h5s(id));
}

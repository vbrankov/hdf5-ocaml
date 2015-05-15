#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "hdf5_hl.h"
#include "h5_stubs.h"
#include "h5i_stubs.h"
#include "h5t_stubs.h"
#include "loc_stubs.h"

void hdf5_h5lt_make_dataset(value loc_id_v, value dset_name_v, value dims_v,
  value type_id_v, value buffer_v)
{
  CAMLparam5(loc_id_v, dset_name_v, dims_v, type_id_v, buffer_v);
  int rank;
  hsize_t *dims;
  herr_t err;

  rank = hsize_t_array_val(dims_v, &dims);
  if (dims == NULL)
    caml_raise_out_of_memory();
  err = H5LTmake_dataset(Loc_val(loc_id_v), String_val(dset_name_v), rank, dims,
    H5T_val(type_id_v), Caml_ba_data_val(buffer_v));
  free(dims);
  raise_if_fail(err);
  CAMLreturn0;
}

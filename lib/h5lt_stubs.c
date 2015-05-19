#include <caml/alloc.h>
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

void hdf5_h5lt_read_dataset_int(value loc_id_v, value dset_name_v, value buffer_v)
{
  CAMLparam3(loc_id_v, dset_name_v, buffer_v);
  raise_if_fail(H5LTread_dataset_int(Loc_val(loc_id_v), String_val(dset_name_v),
    Caml_ba_data_val(buffer_v)));
  CAMLreturn0;
}

value hdf5_h5lt_get_dataset_info(value loc_id_v, value dset_name_v)
{
  CAMLparam2(loc_id_v, dset_name_v);
  CAMLlocal1(info);
  hid_t loc_id = Loc_val(loc_id_v);
  const char *dset_name = String_val(dset_name_v);
  int rank;
  hsize_t *dims;
  H5T_class_t class_id;
  size_t type_size;
  herr_t err;
  
  raise_if_fail(H5LTget_dataset_ndims(loc_id, dset_name, &rank));
  dims = calloc(rank, sizeof(hsize_t));
  if (dims == NULL)
    caml_raise_out_of_memory();
  err = H5LTget_dataset_info(loc_id, dset_name, dims, &class_id, &type_size);
  if (err < 0)
  {
    free(dims);
    fail();
  }
  info = caml_alloc_tuple(3);
  Store_field(info, 0, val_hsize_t_array(rank, dims));
  Store_field(info, 1, Val_h5t_class(class_id));
  Store_field(info, 2, Val_int(type_size));
  CAMLreturn(info);
}

void hdf5_h5lt_set_attribute_int(value loc_id_v, value obj_name_v, value attr_name_v,
  value buffer_v)
{
  CAMLparam4(loc_id_v, obj_name_v, attr_name_v, buffer_v);
  raise_if_fail(H5LTset_attribute_int(Loc_val(loc_id_v), String_val(obj_name_v),
    String_val(attr_name_v), Caml_ba_data_val(buffer_v),
    Caml_ba_array_val(buffer_v)->dim[0]));
  CAMLreturn0;
}

void hdf5_h5lt_get_attribute_int(value loc_id_v, value obj_name_v, value attr_name_v,
  value data_v)
{
  CAMLparam4(loc_id_v, obj_name_v, attr_name_v, data_v);
  raise_if_fail(H5LTget_attribute_int(Loc_val(loc_id_v), String_val(obj_name_v),
    String_val(attr_name_v), Caml_ba_data_val(data_v)));
  CAMLreturn0;
}

#include <assert.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5_stubs.h"
#include "h5a_stubs.h"
#include "h5d_stubs.h"
#include "h5i_stubs.h"
#include "h5g_stubs.h"
#include "h5p_stubs.h"
#include "h5s_stubs.h"
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

H5A_info_t H5A_info_val(value info_v)
{
  CAMLparam1(info_v);
  H5A_info_t info = {
    Int_val(Field(info_v, 0)),
    Int_val(Field(info_v, 1)),
    H5T_cset_val(Field(info_v, 2)),
    Int_val(Field(info_v, 3)) };
  CAMLreturnT(H5A_info_t, info);
}

value Val_h5a_info(H5A_info_t info)
{
  CAMLparam0();
  CAMLlocal1(info_v);
  info_v = caml_alloc_tuple(4);
  Store_field(info_v, 0, Val_int(info.corder_valid));
  Store_field(info_v, 1, Val_int(info.corder));
  Store_field(info_v, 2, Val_h5t_cset(info.cset));
  Store_field(info_v, 3, Val_int(info.data_size));
  CAMLreturn(info_v);
}

value hdf5_h5a_create(value loc_id_v, value attr_name_v, value type_id_v, value acpl_id_v,
  value aapl_id_v, value space_id_v)
{
  CAMLparam5(loc_id_v, attr_name_v, type_id_v, acpl_id_v, aapl_id_v);
  CAMLxparam1(space_id_v);
  CAMLreturn(alloc_h5a(H5Acreate2(Loc_val(loc_id_v), String_val(attr_name_v),
    H5T_val(type_id_v), H5S_val(space_id_v), H5P_opt_val(acpl_id_v),
    H5P_opt_val(aapl_id_v))));
}

value hdf5_h5a_create_bytecode(value *argv, int argn)
{
  assert(argn == 6);
  return hdf5_h5a_create(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value hdf5_h5a_open_name(value loc_id_v, value name_v)
{
  CAMLparam2(loc_id_v, name_v);
  CAMLreturn(alloc_h5a(H5Aopen_name(Loc_val(loc_id_v), String_val(name_v))));
}

value hdf5_h5a_open_idx(value loc_id_v, value idx_v)
{
  CAMLparam2(loc_id_v, idx_v);
  CAMLreturn(alloc_h5a(H5Aopen_idx(Loc_val(loc_id_v), Int_val(idx_v))));
}

void hdf5_h5a_write(value attr_id_v, value mem_type_id_v, value buf_v)
{
  CAMLparam3(attr_id_v, mem_type_id_v, buf_v);
  const void* buf;
  if (Is_long(buf_v))
    caml_invalid_argument("H5a.write: immediate values not allowed");
  else if (Tag_hd(Hd_val(buf_v)) == Custom_tag && Custom_ops_val(buf_v) == caml_ba_ops)
    buf = Caml_ba_data_val(buf_v);
  else
    buf = (const void*) buf_v;
  raise_if_fail(H5Awrite(H5A_val(attr_id_v), H5T_val(mem_type_id_v), buf));
  CAMLreturn0;
}

void hdf5_h5a_read(value attr_id_v, value mem_type_id_v, value buf_v)
{
  CAMLparam3(attr_id_v, mem_type_id_v, buf_v);
  void* buf;
  if (Is_long(buf_v))
    caml_invalid_argument("H5a.read: immediate values not allowed");
  else if (Tag_hd(Hd_val(buf_v)) == Custom_tag && Custom_ops_val(buf_v) == caml_ba_ops)
    buf = Caml_ba_data_val(buf_v);
  else
    buf = (void*) buf_v;
  raise_if_fail(H5Aread(H5A_val(attr_id_v), H5T_val(mem_type_id_v), buf));
  CAMLreturn0;
}

void hdf5_h5a_read_vl(value attr_id_v, value mem_type_id_v, value buf_v)
{
  CAMLparam3(attr_id_v, mem_type_id_v, buf_v);
  void* buf;
  char* s;
  mlsize_t i;
  if (Is_long(buf_v))
    caml_invalid_argument("H5a.read: immediate values not allowed");
  else if (Tag_hd(Hd_val(buf_v)) == Custom_tag && Custom_ops_val(buf_v) == caml_ba_ops)
    caml_invalid_argument("H5a.read: bigarrays not allowed");
  else
    buf = (void*) buf_v;
  raise_if_fail(H5Aread(H5A_val(attr_id_v), H5T_val(mem_type_id_v), buf));
  for (i = 0; i < Wosize_val(buf_v); i++)
  {
    s = ((char**) buf)[i];
    Store_field(buf_v, i, caml_copy_string(s));
    free(s);
  }
  CAMLreturn0;
}

void hdf5_h5a_close(value attr_id_v)
{
  CAMLparam1(attr_id_v);
  raise_if_fail(H5Aclose(H5A_val(attr_id_v)));
  CAMLreturn0;
}

struct operator_data {
  value *callback;
  value *operator_data;
  value *exception;
};

herr_t hdf5_h5a_operator(hid_t location_id, const char *attr_name,
  const H5A_info_t *ainfo, void *op_data)
{
  CAMLparam0();
  CAMLlocal1(ret);
  CAMLlocalN(args, 4);
  struct operator_data *operator_data = op_data;

  args[0] = alloc_loc(location_id);
  args[1] = caml_copy_string(attr_name);
  args[2] = Val_h5a_info(*ainfo);
  args[3] = *operator_data->operator_data;
  ret = caml_callbackN_exn(*operator_data->callback, 4, args);
  if (Is_exception_result(ret))
  {
    *(operator_data->exception) = Extract_exception(ret);
    return -1;
  }
  CAMLreturnT(herr_t, H5_iter_val(ret));
}

void hdf5_h5a_iterate(value obj_id_v, value idx_type_opt_v, value order_opt_v, value n_v,
  value op_v, value op_data_v)
{
  CAMLparam5(obj_id_v, idx_type_opt_v, order_opt_v, n_v, op_v);
  CAMLxparam1(op_data_v);
  CAMLlocal1(exception);

  struct operator_data op = { &op_v, &op_data_v, &exception };
  hsize_t n = Is_block(n_v) ? Int_val(Field(Field(n_v, 0), 0)) : 0;
  exception = Val_unit;

  (void) H5Aiterate(Loc_val(obj_id_v), H5_index_opt_val(idx_type_opt_v),
    H5_iter_order_opt_val(order_opt_v), Is_block(n_v) ? &n : NULL, hdf5_h5a_operator,
    &op);
  if (Is_block(n_v))
    Store_field(Field(n_v, 0), 0, Val_int(n));
  if (exception != Val_unit)
    caml_raise(exception);
  CAMLreturn0;
}

void hdf5_h5a_iterate_bytecode(value *argv, int argn)
{
  assert(argn == 6);
  hdf5_h5a_iterate(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value hdf5_h5a_get_space(value attr_id_v)
{
  CAMLparam1(attr_id_v);
  CAMLreturn(alloc_h5s(H5Aget_space(H5A_val(attr_id_v))));
}

value hdf5_h5a_get_type(value attr_id_v)
{
  CAMLparam1(attr_id_v);
  CAMLreturn(alloc_h5t(H5Aget_type(H5A_val(attr_id_v))));
}

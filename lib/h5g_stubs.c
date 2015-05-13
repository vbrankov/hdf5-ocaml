#include <assert.h>
#include <stdio.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5_stubs.h"
#include "h5g_stubs.h"
#include "h5i_stubs.h"
#include "h5l_stubs.h"
#include "h5p_stubs.h"
#include "loc_stubs.h"

static struct custom_operations h5g_ops = {
  "hdf5.h5g",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_h5g(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&h5g_ops, sizeof(hid_t), 0, 1);
  H5G_val(v) = id;
  return v;
}

void hdf5_h5g_close(value group_v)
{
  CAMLparam1(group_v);
  raise_if_fail(H5Gclose(H5G_val(group_v)));
  CAMLreturn0;
}

value hdf5_h5g_create(value loc_v, value lcpl_v, value gcpl_v, value gapl_v, value name_v)
{
  CAMLparam5(loc_v, lcpl_v, gcpl_v, gapl_v, name_v);
  CAMLreturn(alloc_h5g(H5Gcreate2(Loc_val(loc_v), String_val(name_v), H5P_opt_val(lcpl_v),
    H5P_opt_val(gcpl_v), H5P_opt_val(gapl_v))));
}

value hdf5_h5g_open(value loc_v, value gapl_v, value name_v)
{
  CAMLparam3(loc_v, gapl_v, name_v);
  CAMLreturn(alloc_h5g(H5Gopen2(Loc_val(loc_v), String_val(name_v),
    H5P_opt_val(gapl_v))));
}

void hdf5_h5g_link(value loc_v, value link_type_v, value current_name_v,
  value new_name_v)
{
  CAMLparam4(loc_v, link_type_v, current_name_v, new_name_v);
  raise_if_fail(H5Glink(Loc_val(loc_v), H5L_type_val(link_type_v),
    String_val(current_name_v), String_val(new_name_v)));
  CAMLreturn0;
}

void hdf5_h5g_unlink(value loc_id_v, value name_v)
{
  CAMLparam2(loc_id_v, name_v);
  raise_if_fail(H5Gunlink(Loc_val(loc_id_v), String_val(name_v)));
  CAMLreturn0;
}

void hdf5_h5g_set_comment(value loc_id_v, value name_v, value comment_v)
{
  CAMLparam3(loc_id_v, name_v, comment_v);
  raise_if_fail(H5Gset_comment(Loc_val(loc_id_v), String_val(name_v),
    String_val(comment_v)));
  CAMLreturn0;
}

value hdf5_h5g_get_comment(value loc_id_v, value name_v)
{
  CAMLparam2(loc_id_v, name_v);
  CAMLlocal1(v);
  int bufsize;

  bufsize = H5Gget_comment(Loc_val(loc_id_v), String_val(name_v), 0, NULL);
  v = caml_alloc_string(bufsize);
  H5Gget_comment(Loc_val(loc_id_v), String_val(name_v), bufsize, String_val(v));
  CAMLreturn(v);
}

struct operator_data {
  value callback;
  value operator_data;
  value *exception;
};

herr_t hdf5_h5g_operator(hid_t group, const char *name, void *op_data)
{
  CAMLparam0();
  CAMLlocal1(ret);
  struct operator_data *operator_data = op_data;
  ret = caml_callback3_exn(operator_data->callback, alloc_h5g(group),
    caml_copy_string(name), operator_data->operator_data);
  if (Is_exception_result(ret))
  {
    *(operator_data->exception) = Extract_exception(ret);
    return -1;
  }
  CAMLreturnT(herr_t, H5_iter_val(ret));
}

void hdf5_h5g_iterate(value loc_id_v, value name_v, value idx_v, value operator_v,
  value operator_data_v)
{
  CAMLparam5(loc_id_v, name_v, idx_v, operator_v, operator_data_v);
  CAMLlocal1(exception);

  struct operator_data operator_data = { operator_v, operator_data_v, &exception };
  int idx = Is_block(idx_v) ? Int_val(Field(Field(idx_v, 0), 0)) : 0;
  exception = Val_unit;

  (void) H5Giterate(Loc_val(loc_id_v), String_val(name_v),
        Is_block(idx_v) ? &idx : NULL, hdf5_h5g_operator, &operator_data);
  if (Is_block(idx_v))
    Store_field(Field(idx_v, 0), 0, Val_int(idx));
  if (exception != Val_unit)
    caml_raise(exception);
  CAMLreturn0;
}

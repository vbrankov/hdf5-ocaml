#include <stddef.h>
#include <stdlib.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include "hdf5.h"
#include "h5i_stubs.h"

int hsize_t_array_val(value v, hsize_t **a)
{
  long i, length, e;

  length = Wosize_val(v);
  *a = calloc(length, sizeof(hsize_t));
  if (*a == NULL)
    return length;
  for (i = 0; i < length; i++)
  {
    e = Long_val(Field(v, i));
    (*a)[i] = e <= -1 ? H5S_UNLIMITED : (hsize_t) e;
  }
  return length;
}

int hsize_t_array_opt_val(value v, hsize_t **a)
{
  if (Is_block(v))
    return hsize_t_array_val(Field(v, 0), a);
  (*a) = NULL;
  return 0;
}

value val_hsize_t_array(int length, hsize_t *a)
{
  CAMLparam0();
  CAMLlocal1(a_v);
  int i;

  a_v = caml_alloc_tuple(length);
  for (i = 0; i < length; i++)
    Field(a_v, i) = Val_int(a[i]);
  CAMLreturn(a_v);
}

H5_iter_order_t H5_iter_order_val(value iter_order)
{
  switch (Int_val(iter_order))
  {
    case 0: return H5_ITER_INC;
    case 1: return H5_ITER_DEC;
    case 2: return H5_ITER_NATIVE;
    case 3: return H5_ITER_N;
    default: caml_failwith("unrecognized H5_iter_order_t");
  }
}

H5_iter_order_t H5_iter_order_opt_val(value v)
{
  if (Is_block(v))
    return H5_iter_order_val(Field(v, 0));
  return H5_ITER_INC;
}

value Val_h5_iter_order(H5_iter_order_t iter_order)
{
  switch (iter_order)
  {
    case H5_ITER_UNKNOWN: fail();
    case H5_ITER_INC:     return Val_int(0);
    case H5_ITER_DEC:     return Val_int(1);
    case H5_ITER_NATIVE:  return Val_int(2);
    case H5_ITER_N:       return Val_int(3);
    default: caml_failwith("unrecognized H5_iter_order_t");
  }
}

int H5_iter_val(value iter)
{
  switch (Int_val(iter))
  {
    case 0: return H5_ITER_CONT;
    case 1: return H5_ITER_STOP;
    default: caml_failwith("unrecognized H5_iter_t");
  }
}

value Val_h5_iter(int iter)
{
  switch (iter)
  {
    case H5_ITER_ERROR: fail();
    case H5_ITER_CONT:  return Val_int(0);
    case H5_ITER_STOP:  return Val_int(1);
    default: caml_failwith("unrecognized H5_iter_t");
  }
}

H5_index_t H5_index_val(value index)
{
  switch (Int_val(index))
  {
    case 0: return H5_INDEX_NAME;
    case 1: return H5_INDEX_CRT_ORDER;
    case 2: return H5_INDEX_N;
    default: caml_failwith("unrecognized H5_index_t");
  }
}

H5_index_t H5_index_opt_val(value v)
{
  if (Is_block(v))
    return H5_index_val(Field(v, 0));
  return H5_INDEX_NAME;
}

value Val_h5_index(H5_index_t index)
{
  switch (index)
  {
    case H5_INDEX_UNKNOWN:   fail();
    case H5_INDEX_NAME:      return Val_int(0);
    case H5_INDEX_CRT_ORDER: return Val_int(1);
    case H5_INDEX_N:         return Val_int(2);
    default: caml_failwith("unrecognized H5_index_t");
  }
}

H5_ih_info_t H5_ih_info_val(value ih_info_v)
{
  CAMLparam1(ih_info_v);
  H5_ih_info_t ih_info = {
    Int_val(Field(ih_info_v, 0)),
    Int_val(Field(ih_info_v, 1)) };
  CAMLreturnT(H5_ih_info_t, ih_info);
}

value Val_h5_ih_info(H5_ih_info_t ih_info)
{
  CAMLparam0();
  CAMLlocal1(ih_info_v);
  ih_info_v = caml_alloc_tuple(2);
  Store_field(ih_info_v, 0, Val_int(ih_info.index_size));
  Store_field(ih_info_v, 1, Val_int(ih_info.heap_size));
  CAMLreturn(ih_info_v);
}

value Val_htri(htri_t htri)
{
  if (htri < 0)
    fail();
  return Val_bool(htri);
}

struct custom_operations *caml_ba_ops;

void hdf5_h5_init()
{
  caml_ba_ops = Custom_ops_val(
    caml_ba_alloc_dims(CAML_BA_FLOAT64 | CAML_BA_C_LAYOUT | CAML_BA_MANAGED, 1, NULL, 1));
}

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5_stubs.h"
#include "h5i_stubs.h"
#include "h5o_stubs.h"
#include "h5p_stubs.h"
#include "loc_stubs.h"

H5O_type_t H5O_type_val(value type)
{
  switch (Int_val(type))
  {
    case 0: return H5O_TYPE_GROUP;
    case 1: return H5O_TYPE_DATASET;
    case 2: return H5O_TYPE_NAMED_DATATYPE;
    case 3: return H5O_TYPE_NTYPES;
    default: caml_failwith("unrecognized H5O_type_t");
  }
}

value Val_h5o_type(H5O_type_t type)
{
  switch (type)
  {
    case H5O_TYPE_UNKNOWN:        fail();
    case H5O_TYPE_GROUP:          return Val_int(0);
    case H5O_TYPE_DATASET:        return Val_int(1);
    case H5O_TYPE_NAMED_DATATYPE: return Val_int(2);
    case H5O_TYPE_NTYPES:         return Val_int(3);
    default: caml_failwith("unrecognized H5O_type_t");
  }
}

H5O_hdr_info_t H5O_hdr_info_val(value hdr_info_v)
{
  CAMLparam1(hdr_info_v);
  CAMLlocal2(space_v, mesg_v);
  space_v = Field(hdr_info_v, 4);
  mesg_v  = Field(hdr_info_v, 5);
  H5O_hdr_info_t hdr_info = {
    Int_val(Field(hdr_info_v, 0)),
    Int_val(Field(hdr_info_v, 1)),
    Int_val(Field(hdr_info_v, 2)),
    Int_val(Field(hdr_info_v, 3)),
    { Int_val(Field(space_v, 0)),
      Int_val(Field(space_v, 1)),
      Int_val(Field(space_v, 2)),
      Int_val(Field(space_v, 3)) },
    { Int_val(Field(mesg_v, 0)),
      Int_val(Field(mesg_v, 1)) } };
  CAMLreturnT(H5O_hdr_info_t, hdr_info);
}

value Val_h5o_hdr_info(const H5O_hdr_info_t *hdr_info)
{
  CAMLparam0();
  CAMLlocal3(hdr_info_v, space_v, mesg_v);
  hdr_info_v = caml_alloc_tuple(6);
  space_v    = caml_alloc_tuple(4);
  mesg_v     = caml_alloc_tuple(2);
  Store_field(hdr_info_v, 0, Val_int(hdr_info->version));
  Store_field(hdr_info_v, 1, Val_int(hdr_info->nmesgs));
  Store_field(hdr_info_v, 2, Val_int(hdr_info->nchunks));
  Store_field(hdr_info_v, 3, Val_int(hdr_info->flags));
  Store_field(hdr_info_v, 4, space_v);
  Store_field(hdr_info_v, 5, mesg_v);
  Store_field(space_v, 0, Val_int(hdr_info->space.total));
  Store_field(space_v, 1, Val_int(hdr_info->space.meta));
  Store_field(space_v, 2, Val_int(hdr_info->space.mesg));
  Store_field(space_v, 3, Val_int(hdr_info->space.free));
  Store_field(mesg_v, 0, Val_int(hdr_info->mesg.present));
  Store_field(mesg_v, 1, Val_int(hdr_info->mesg.shared));
  CAMLreturn(hdr_info_v);
}

H5O_info_t H5O_info_val(value info_v)
{
  CAMLparam1(info_v);
  CAMLlocal1(meta_size_v);
  meta_size_v = Field(info_v, 10);
  H5O_info_t info = {
    Int_val(Field(info_v, 0)),
    Loc_val(Field(info_v, 1)),
    H5O_type_val(Field(info_v, 2)),
    Int_val(Field(info_v, 3)),
    Int64_val(Field(info_v, 4)),
    Int64_val(Field(info_v, 5)),
    Int64_val(Field(info_v, 6)),
    Int64_val(Field(info_v, 7)),
    Int_val(Field(info_v, 8)),
    H5O_hdr_info_val(Field(info_v, 9)),
    { H5_ih_info_val(Field(meta_size_v, 0)),
      H5_ih_info_val(Field(meta_size_v, 1)) } };
  CAMLreturnT(H5O_info_t, info);
}

value Val_h5o_info(const H5O_info_t *info)
{
  CAMLparam0();
  CAMLlocal2(info_v, meta_size_v);
  info_v      = caml_alloc_tuple(10);
  meta_size_v = caml_alloc_tuple(2);
  Store_field(info_v, 0, Val_int(info->fileno));
  Store_field(info_v, 1, info->addr);
  Store_field(info_v, 2, Val_h5o_type(info->type));
  Store_field(info_v, 3, Val_int(info->rc));
  Store_field(info_v, 4, caml_copy_int64(info->atime));
  Store_field(info_v, 5, caml_copy_int64(info->mtime));
  Store_field(info_v, 6, caml_copy_int64(info->ctime));
  Store_field(info_v, 7, caml_copy_int64(info->btime));
  Store_field(info_v, 8, Val_int(info->num_attrs));
  Store_field(info_v, 9, Val_h5o_hdr_info(&info->hdr));
  Store_field(info_v, 10, meta_size_v);
  Store_field(meta_size_v, 0, Val_h5_ih_info(info->meta_size.obj));
  Store_field(meta_size_v, 1, Val_h5_ih_info(info->meta_size.attr));
  CAMLreturn(info_v);
}

value hdf5_h5o_get_info(value object_v)
{
  CAMLparam1(object_v);
  H5O_info_t object_info;
  raise_if_fail(H5Oget_info(Loc_val(object_v), &object_info));
  CAMLreturn(Val_h5o_info(&object_info));
}

value hdf5_h5o_get_info_by_name(value loc_v, value lapl_v, value object_name_v)
{
  CAMLparam3(loc_v, lapl_v, object_name_v);
  H5O_info_t object_info;
  raise_if_fail(H5Oget_info_by_name(Loc_val(loc_v), String_val(object_name_v),
    &object_info, H5P_opt_val(lapl_v)));
  CAMLreturn(Val_h5o_info(&object_info));
}

#include <assert.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5i_stubs.h"
#include "h5p_stubs.h"
#include "h5t_stubs.h"
#include "loc_stubs.h"

static struct custom_operations h5t_ops = {
  "hdf5.h5t",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value alloc_h5t(hid_t id)
{
  raise_if_fail(id);
  value v = caml_alloc_custom(&h5t_ops, sizeof(hid_t), 0, 1);
  H5T_val(v) = id;
  return v;
}

H5T_class_t H5T_class_val(value class)
{
  switch (Int_val(class))
  {
    case  0: return H5T_NO_CLASS;
    case  1: return H5T_INTEGER;
    case  2: return H5T_FLOAT;
    case  3: return H5T_TIME;
    case  4: return H5T_STRING;
    case  5: return H5T_BITFIELD;
    case  6: return H5T_OPAQUE;
    case  7: return H5T_COMPOUND;
    case  8: return H5T_REFERENCE;
    case  9: return H5T_ENUM;
    case 10: return H5T_VLEN;
    case 11: return H5T_ARRAY;
    case 12: return H5T_NCLASSES;
    default: caml_failwith("unrecognized H5T_class_t");
  }
}

value Val_h5t_class(H5T_class_t class)
{
  switch (class)
  {
    case H5T_NO_CLASS:  return Val_int( 0);
    case H5T_INTEGER:   return Val_int( 1);
    case H5T_FLOAT:     return Val_int( 2);
    case H5T_TIME:      return Val_int( 3);
    case H5T_STRING:    return Val_int( 4);
    case H5T_BITFIELD:  return Val_int( 5);
    case H5T_OPAQUE:    return Val_int( 6);
    case H5T_COMPOUND:  return Val_int( 7);
    case H5T_REFERENCE: return Val_int( 8);
    case H5T_ENUM:      return Val_int( 9);
    case H5T_VLEN:      return Val_int(10);
    case H5T_ARRAY:     return Val_int(11);
    case H5T_NCLASSES:  return Val_int(12);
    default: caml_failwith("unrecognized H5T_class_t");
  }
}

H5T_order_t H5T_order_val(value order)
{
  switch (Int_val(order))
  {
    case  0: return H5T_ORDER_ERROR;
    case  1: return H5T_ORDER_LE;
    case  2: return H5T_ORDER_BE;
    case  3: return H5T_ORDER_VAX;
    case  4: return H5T_ORDER_NONE;
    default: caml_failwith("unrecognized H5T_order_t");
  }
}

value Val_h5t_order(H5T_order_t order)
{
  switch (order)
  {
    case H5T_ORDER_ERROR: return Val_int(0);
    case H5T_ORDER_LE:    return Val_int(1);
    case H5T_ORDER_BE:    return Val_int(2);
    case H5T_ORDER_VAX:   return Val_int(3);
    case H5T_ORDER_NONE:  return Val_int(4);
    default: caml_failwith("unrecognized H5T_order_t");
  }
}

H5T_sign_t H5T_sign_val(value sign)
{
  switch (Int_val(sign))
  {
    case 0: return H5T_SGN_NONE;
    case 1: return H5T_SGN_2;
    case 2: return H5T_NSGN;
    default: caml_failwith("unrecognized H5T_sign_t");
  }
}

value Val_h5t_sign(H5T_sign_t sign)
{
  switch (sign)
  {
    case H5T_SGN_ERROR: fail();
    case H5T_SGN_NONE:  return Val_int(0);
    case H5T_SGN_2:     return Val_int(1);
    case H5T_NSGN:      return Val_int(2);
    default: caml_failwith("unrecognized H5T_sign_t");
  }
}

H5T_norm_t H5T_norm_val(value norm)
{
  switch (Int_val(norm))
  {
    case 0: return H5T_NORM_IMPLIED;
    case 1: return H5T_NORM_MSBSET;
    case 2: return H5T_NORM_NONE;
    default: caml_failwith("unrecognized H5T_norm_t");
  }
}

value Val_h5t_norm(H5T_norm_t norm)
{
  switch (norm)
  {
    case H5T_NORM_ERROR:   fail();
    case H5T_NORM_IMPLIED: return Val_int(0);
    case H5T_NORM_MSBSET:  return Val_int(1);
    case H5T_NORM_NONE:    return Val_int(2);
    default: caml_failwith("unrecognized H5T_norm_t");
  }
}

H5T_cset_t H5T_cset_val(value cset)
{
  switch (Int_val(cset))
  {
    case 0: return H5T_CSET_ASCII;
    case 1: return H5T_CSET_UTF8;
    default: caml_failwith("unrecognized H5T_cset_t");
  }
}

value Val_h5t_cset(H5T_cset_t cset)
{
  switch (cset)
  {
    case H5T_CSET_ERROR: fail();
    case H5T_CSET_ASCII: return Val_int(0);
    case H5T_CSET_UTF8:  return Val_int(1);
    default: caml_failwith("unrecognized H5T_cset_t");
  }
}

H5T_str_t H5T_str_val(value str)
{
  switch (Int_val(str))
  {
    case 0: return H5T_STR_NULLTERM;
    case 1: return H5T_STR_NULLPAD;
    case 2: return H5T_STR_SPACEPAD;
    default: caml_failwith("unrecognized H5T_str_t");
  }
}

value Val_h5t_str(H5T_str_t str)
{
  switch (str)
  {
    case H5T_STR_ERROR:    fail();
    case H5T_STR_NULLTERM: return Val_int(0);
    case H5T_STR_NULLPAD:  return Val_int(1);
    case H5T_STR_SPACEPAD: return Val_int(2);
    default: caml_failwith("unrecognized H5T_str_t");
  }
}

H5T_pad_t H5T_pad_val(value pad)
{
  switch (Int_val(pad))
  {
    case 0: return H5T_PAD_ZERO;
    case 1: return H5T_PAD_ONE;
    case 2: return H5T_PAD_BACKGROUND;
    case 3: return H5T_NPAD;
    default: caml_failwith("unrecognized H5T_pad_t");
  }
}

value Val_h5t_pad(H5T_pad_t pad)
{
  switch (pad)
  {
    case H5T_PAD_ERROR:      fail();
    case H5T_PAD_ZERO:       return Val_int(0);
    case H5T_PAD_ONE:        return Val_int(1);
    case H5T_PAD_BACKGROUND: return Val_int(2);
    case H5T_NPAD:           return Val_int(3);
    default: caml_failwith("unrecognized H5T_pad_t");
  }
}

H5T_cmd_t H5T_cmd_val(value cmd)
{
  switch (Int_val(cmd))
  {
    case 0: return H5T_CONV_INIT;
    case 1: return H5T_CONV_CONV;
    case 2: return H5T_CONV_FREE;
    default: caml_failwith("unrecognized H5T_cmd_t");
  }
}

value Val_h5t_cmd(H5T_cmd_t cmd)
{
  switch (cmd)
  {
    case H5T_CONV_INIT: return Val_int(0);
    case H5T_CONV_CONV: return Val_int(1);
    case H5T_CONV_FREE: return Val_int(2);
    default: caml_failwith("unrecognized H5T_cmd_t");
  }
}

H5T_bkg_t H5T_bkg_val(value bkg)
{
  switch (Int_val(bkg))
  {
    case 0: return H5T_BKG_NO;
    case 1: return H5T_BKG_TEMP;
    case 2: return H5T_BKG_YES;
    default: caml_failwith("unrecognized H5T_bkg_t");
  }
}

value Val_h5t_bkg(H5T_bkg_t bkg)
{
  switch (bkg)
  {
    case H5T_BKG_NO:   return Val_int(0);
    case H5T_BKG_TEMP: return Val_int(1);
    case H5T_BKG_YES:  return Val_int(2);
    default: caml_failwith("unrecognized H5T_bkg_t");
  }
}

H5T_cdata_t H5T_cdata_val(value cdata_v)
{
  CAMLparam1(cdata_v);
  H5T_cdata_t cdata = {
    H5T_cmd_val(Field(cdata_v, 0)),
    H5T_bkg_val(Field(cdata_v, 1)),
    Int_val(Field(cdata_v, 2)),
    (void*) Int64_val(Field(cdata_v, 3)) };
  CAMLreturnT(H5T_cdata_t, cdata);
}

value Val_h5t_cdata(H5T_cdata_t cdata)
{
  CAMLparam0();
  CAMLlocal1(cdata_v);
  cdata_v = caml_alloc_tuple(4);
  Store_field(cdata_v, 0, Val_h5t_cmd(cdata.command));
  Store_field(cdata_v, 1, Val_h5t_bkg(cdata.need_bkg));
  Store_field(cdata_v, 2, Val_int(cdata.recalc));
  Store_field(cdata_v, 3, caml_copy_int64((int64_t) cdata.priv));
  CAMLreturn(cdata_v);
}

H5T_pers_t H5T_pers_val(value pers)
{
  switch (Int_val(pers))
  {
    case 0: return H5T_PERS_DONTCARE;
    case 1: return H5T_PERS_HARD;
    case 2: return H5T_PERS_SOFT;
    default: caml_failwith("unrecognized H5T_pers_t");
  }
}

value Val_h5t_pers(H5T_pers_t pers)
{
  switch (pers)
  {
    case H5T_PERS_DONTCARE: return Val_int(0);
    case H5T_PERS_HARD:     return Val_int(1);
    case H5T_PERS_SOFT:     return Val_int(2);
    default: caml_failwith("unrecognized H5T_pers_t");
  }
}

H5T_direction_t H5T_direction_val(value direction)
{
  switch (Int_val(direction))
  {
    case 0: return H5T_DIR_DEFAULT;
    case 1: return H5T_DIR_ASCEND;
    case 2: return H5T_DIR_DESCEND;
    default: caml_failwith("unrecognized H5T_direction_t");
  }
}

value Val_h5t_direction(H5T_direction_t direction)
{
  switch (direction)
  {
    case H5T_DIR_DEFAULT: return Val_int(0);
    case H5T_DIR_ASCEND:  return Val_int(1);
    case H5T_DIR_DESCEND: return Val_int(2);
    default: caml_failwith("unrecognized H5T_direction_t");
  }
}

H5T_conv_except_t H5T_conv_except_val(value conv_except)
{
  switch (Int_val(conv_except))
  {
    case 0: return H5T_CONV_EXCEPT_RANGE_HI;
    case 1: return H5T_CONV_EXCEPT_RANGE_LOW;
    case 2: return H5T_CONV_EXCEPT_PRECISION;
    case 3: return H5T_CONV_EXCEPT_TRUNCATE;
    case 4: return H5T_CONV_EXCEPT_PINF;
    case 5: return H5T_CONV_EXCEPT_NINF;
    case 6: return H5T_CONV_EXCEPT_NAN;
    default: caml_failwith("unrecognized H5T_conv_except_t");
  }
}

value Val_h5t_conv_except(H5T_conv_except_t conv_except)
{
  switch (conv_except)
  {
    case H5T_CONV_EXCEPT_RANGE_HI:  return Val_int(0);
    case H5T_CONV_EXCEPT_RANGE_LOW: return Val_int(1);
    case H5T_CONV_EXCEPT_PRECISION: return Val_int(2);
    case H5T_CONV_EXCEPT_TRUNCATE:  return Val_int(3);
    case H5T_CONV_EXCEPT_PINF:      return Val_int(4);
    case H5T_CONV_EXCEPT_NINF:      return Val_int(5);
    case H5T_CONV_EXCEPT_NAN:       return Val_int(6);
    default: caml_failwith("unrecognized H5T_conv_except_t");
  }
}

H5T_conv_ret_t H5T_conv_ret_val(value conv_ret)
{
  switch (Int_val(conv_ret))
  {
    case 0: return H5T_CONV_ABORT;
    case 1: return H5T_CONV_UNHANDLED;
    case 2: return H5T_CONV_HANDLED;
    default: caml_failwith("unrecognized H5T_conv_ret_t");
  }
}

value Val_h5t_conv_ret(H5T_conv_ret_t conv_ret)
{
  switch (conv_ret)
  {
    case H5T_CONV_ABORT:     return Val_int(0);
    case H5T_CONV_UNHANDLED: return Val_int(1);
    case H5T_CONV_HANDLED:   return Val_int(2);
    default: caml_failwith("unrecognized H5T_conv_ret_t");
  }
}

value hdf5_h5t_get_variable(value unit_v)
{
  CAMLparam1(unit_v);
  CAMLreturn(Val_int(H5T_VARIABLE));
}

value hdf5_h5t_datatypes(value unit_v)
{
  CAMLparam1(unit_v);
  int i;

  CAMLlocal1(v);
  hid_t datatypes[] = { H5T_IEEE_F32BE, H5T_IEEE_F32LE, H5T_IEEE_F64BE,
    H5T_IEEE_F64LE, H5T_STD_I8BE, H5T_STD_I8LE, H5T_STD_I16BE, H5T_STD_I16LE,
    H5T_STD_I32BE, H5T_STD_I32LE, H5T_STD_I64BE, H5T_STD_I64LE, H5T_STD_U8BE,
    H5T_STD_U8LE, H5T_STD_U16BE, H5T_STD_U16LE, H5T_STD_U32BE, H5T_STD_U32LE,
    H5T_STD_U64BE, H5T_STD_U64LE, H5T_STD_B8BE, H5T_STD_B8LE, H5T_STD_B16BE,
    H5T_STD_B16LE, H5T_STD_B32BE, H5T_STD_B32LE, H5T_STD_B64BE, H5T_STD_B64LE,
    H5T_STD_REF_OBJ, H5T_STD_REF_DSETREG, H5T_UNIX_D32BE, H5T_UNIX_D32LE, H5T_UNIX_D64BE,
    H5T_UNIX_D64LE, H5T_C_S1, H5T_FORTRAN_S1, H5T_INTEL_I8, H5T_INTEL_I16, H5T_INTEL_I32,
    H5T_INTEL_I64, H5T_INTEL_U8, H5T_INTEL_U16, H5T_INTEL_U32, H5T_INTEL_U64,
    H5T_INTEL_B8, H5T_INTEL_B16, H5T_INTEL_B32, H5T_INTEL_B64, H5T_INTEL_F32,
    H5T_INTEL_F64, H5T_ALPHA_I8, H5T_ALPHA_I16, H5T_ALPHA_I32, H5T_ALPHA_I64,
    H5T_ALPHA_U8, H5T_ALPHA_U16, H5T_ALPHA_U32, H5T_ALPHA_U64, H5T_ALPHA_B8,
    H5T_ALPHA_B16, H5T_ALPHA_B32, H5T_ALPHA_B64, H5T_ALPHA_F32, H5T_ALPHA_F64,
    H5T_MIPS_I8, H5T_MIPS_I16, H5T_MIPS_I32, H5T_MIPS_I64, H5T_MIPS_U8, H5T_MIPS_U16,
    H5T_MIPS_U32, H5T_MIPS_U64, H5T_MIPS_B8, H5T_MIPS_B16, H5T_MIPS_B32, H5T_MIPS_B64,
    H5T_MIPS_F32, H5T_MIPS_F64, H5T_VAX_F32, H5T_VAX_F64, H5T_NATIVE_CHAR,
    H5T_NATIVE_SCHAR, H5T_NATIVE_UCHAR, H5T_NATIVE_SHORT, H5T_NATIVE_USHORT,
    H5T_NATIVE_INT, H5T_NATIVE_UINT, H5T_NATIVE_LONG, H5T_NATIVE_ULONG, H5T_NATIVE_LLONG,
    H5T_NATIVE_ULLONG, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE,
    H5T_NATIVE_B8, H5T_NATIVE_B16, H5T_NATIVE_B32, H5T_NATIVE_B64, H5T_NATIVE_OPAQUE,
    H5T_NATIVE_HADDR, H5T_NATIVE_HSIZE, H5T_NATIVE_HSSIZE, H5T_NATIVE_HERR,
    H5T_NATIVE_HBOOL, H5T_NATIVE_INT8, H5T_NATIVE_UINT8, H5T_NATIVE_INT_LEAST8,
    H5T_NATIVE_UINT_LEAST8, H5T_NATIVE_INT_FAST8, H5T_NATIVE_UINT_FAST8, H5T_NATIVE_INT16,
    H5T_NATIVE_UINT16, H5T_NATIVE_INT_LEAST16, H5T_NATIVE_UINT_LEAST16,
    H5T_NATIVE_INT_FAST16, H5T_NATIVE_UINT_FAST16, H5T_NATIVE_INT32, H5T_NATIVE_UINT32,
    H5T_NATIVE_INT_LEAST32, H5T_NATIVE_UINT_LEAST32, H5T_NATIVE_INT_FAST32,
    H5T_NATIVE_UINT_FAST32, H5T_NATIVE_INT64, H5T_NATIVE_UINT64, H5T_NATIVE_INT_LEAST64,
    H5T_NATIVE_UINT_LEAST64, H5T_NATIVE_INT_FAST64, H5T_NATIVE_UINT_FAST64 };
  int len = sizeof(datatypes) / sizeof(hid_t);

  v = caml_alloc_tuple(len);
  for (i = 0; i < len; i++)
    Store_field(v, i, alloc_h5t(datatypes[i]));
  
  CAMLreturn(v);
}

value hdf5_h5t_create(value class_v, value size_v)
{
  CAMLparam2(class_v, size_v);
  CAMLreturn(alloc_h5t(H5Tcreate(H5T_class_val(class_v), Int_val(size_v))));
}

void hdf5_h5t_commit(value loc_id_v, value name_v, value lcpl_id_v, value tcpl_id_v,
  value tapl_id_v, value dtype_id_v)
{
  CAMLparam5(loc_id_v, name_v, lcpl_id_v, tcpl_id_v, tapl_id_v);
  CAMLxparam1(dtype_id_v);
  raise_if_fail(H5Tcommit2(Loc_val(loc_id_v), String_val(name_v), H5T_val(dtype_id_v),
    H5P_opt_val(lcpl_id_v), H5P_opt_val(tcpl_id_v), H5P_opt_val(tapl_id_v)));
  CAMLreturn0;
}

void hdf5_h5t_commit_bytecode(value *argv, int argn)
{
  assert(argn == 6);
  hdf5_h5t_commit(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value hdf5_h5t_copy(value id_v)
{
  CAMLparam1(id_v);
  CAMLreturn(alloc_h5t(H5Tcopy(H5T_val(id_v))));
}

value hdf5_h5t_equal(value dtype_id1_v, value dtype_id2_v)
{
  CAMLparam2(dtype_id1_v, dtype_id2_v);
  htri_t ret;
  ret = H5Tequal(H5T_val(dtype_id1_v), H5T_val(dtype_id2_v));
  raise_if_fail(ret);
  CAMLreturn(Val_bool(ret));
}

value hdf5_h5t_get_class(value dtype_id_v)
{
  CAMLparam1(dtype_id_v);
  CAMLreturn(Val_h5t_class(H5Tget_class(H5T_val(dtype_id_v))));
}

void hdf5_h5t_set_size(value dtype_id_v, value size_v)
{
  CAMLparam2(dtype_id_v, size_v);
  raise_if_fail(H5Tset_size(H5T_val(dtype_id_v), Int_val(size_v)));
  CAMLreturn0;
}

value hdf5_h5t_get_size(value dtype_id_v)
{
  CAMLparam1(dtype_id_v);
  CAMLreturn(Val_int(H5Tget_size(H5T_val(dtype_id_v))));
}

value hdf5_h5t_get_native_type(value dtype_id_v, value direction_v)
{
  CAMLparam2(dtype_id_v, direction_v);
  CAMLreturn(alloc_h5t(H5Tget_native_type(H5T_val(dtype_id_v),
    H5T_direction_val(direction_v))));
}

void hdf5_h5t_close(value dtype_id_v)
{
  CAMLparam1(dtype_id_v);
  raise_if_fail(H5Tclose(H5T_val(dtype_id_v)));
  CAMLreturn0;
}

value hdf5_h5t_get_order(value dtype_id_v)
{
  CAMLparam1(dtype_id_v);
  CAMLreturn(Val_h5t_order(H5Tget_order(H5T_val(dtype_id_v))));
}

void hdf5_h5t_set_order(value id_v, value order_v)
{
  CAMLparam2(id_v, order_v);

  raise_if_fail(H5Tset_order(H5T_val(id_v), H5T_order_val(order_v)));
  CAMLreturn0;
}

value hdf5_h5t_get_strpad(value dtype_id_v)
{
  CAMLparam1(dtype_id_v);
  CAMLreturn(Val_h5t_str(H5Tget_strpad(H5T_val(dtype_id_v))));
}

void hdf5_h5t_set_strpad(value id_v, value strpad_v)
{
  CAMLparam2(id_v, strpad_v);
  raise_if_fail(H5Tset_strpad(H5T_val(id_v), H5T_str_val(strpad_v)));
  CAMLreturn0;
}

value hdf5_h5t_is_variable_str(value dtype_id_v)
{
  CAMLparam1(dtype_id_v);
  CAMLreturn(Val_bool(H5Tis_variable_str(H5T_val(dtype_id_v))));
}

value hdf5_h5t_get_nmembers(value dtype_id_v)
{
  CAMLparam1(dtype_id_v);
  int v = H5Tget_nmembers(H5T_val(dtype_id_v));
  if (v < 0) fail();
  CAMLreturn(Val_int(v));
}

void hdf5_h5t_insert(value dtype_id_v, value name_v, value offset_v, value field_id_v)
{
  CAMLparam4(dtype_id_v, name_v, offset_v, field_id_v);

  raise_if_fail(H5Tinsert(H5T_val(dtype_id_v), String_val(name_v), Int_val(offset_v),
    H5T_val(field_id_v)));
  CAMLreturn0;
}

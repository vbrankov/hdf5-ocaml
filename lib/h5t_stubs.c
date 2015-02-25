#include <assert.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5i_stubs.h"

static struct custom_operations h5t_ops = {
  "hdf5.h5t",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define H5T_val(v) *((hid_t*) Data_custom_val(v))

static value alloc_h5t(hid_t id)
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

H5T_order_t H5T_order_val(value order)
{
  switch (Int_val(order))
  {
    case  0: return H5T_ORDER_ERROR;
    case  1: return H5T_ORDER_LE;
    case  2: return H5T_ORDER_BE;
    case  3: return H5T_ORDER_VAX;
    case  4: return H5T_ORDER_MIXED;
    case  5: return H5T_ORDER_NONE;
    default: caml_failwith("unrecognized H5T_order_t");
  }
}

value caml_h5t_datatypes(value unit_v)
{
  CAMLparam1(unit_v);

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
  for (int i = 0; i < len; i++)
    Store_field(v, i, alloc_h5t(datatypes[i]));
  
  CAMLreturn(v);
}

value caml_h5t_copy(value id_v)
{
  CAMLparam1(id_v);

  CAMLreturn(alloc_h5t(H5Tcopy(H5T_val(id_v))));
}

value caml_h5t_create(value class_v, value size_v)
{
  CAMLparam2(class_v, size_v);

  H5T_class_t class = H5T_class_val(class_v);
  size_t size = Int_val(size_v);

  CAMLreturn(alloc_h5t(H5Tcreate(class, size)));
}

void caml_h5t_set_order(value id_v, value order_v)
{
  CAMLparam2(id_v, order_v);

  raise_if_fail(H5Tset_order(H5T_val(id_v), H5T_order_val(order_v)));
  CAMLreturn0;
}

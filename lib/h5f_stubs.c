#include <assert.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include "hdf5.h"
#include "h5p_stubs.h"

static struct custom_operations h5f_ops = {
  "hdf5.h5f",
  custom_finalize_default,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define H5F_val(v) *((hid_t*) Data_custom_val(v))

static value alloc_h5f(hid_t id)
{
  value v = caml_alloc_custom(&h5f_ops, sizeof(hid_t), 0, 1);
  H5F_val(v) = id;
  return v;
}

unsigned acc_val(value v)
{
  CAMLlocal1(h);
  unsigned flags = 0;

  while (Is_block(v))
  {
    assert(Tag_val(v) == 0);
    h = Field(v, 0);
    v = Field(v, 1);
    assert(Is_long(h));
    switch (Long_val(h))
    {
      case 0: flags |= H5F_ACC_RDONLY; break;
      case 1: flags |= H5F_ACC_RDWR; break;
      case 2: flags |= H5F_ACC_TRUNC; break;
      case 3: flags |= H5F_ACC_EXCL; break;
      case 4: flags |= H5F_ACC_DEBUG; break;
      case 5: flags |= H5F_ACC_CREAT; break;
      case 6: flags |= H5F_ACC_DEFAULT; break;
      default: caml_failwith("unrecognized acc");
    }
  }
  assert(Int_val(v) == 0);
  return flags;
}

value caml_h5f_close(value cls_id_v)
{
  CAMLparam1(cls_id_v);
  CAMLreturn(Val_int(H5Fclose(H5F_val(cls_id_v))));
}

value caml_h5f_create(value name_v, value fcpl_id_v, value fapl_id_v, value flags_v)
{
  CAMLparam4(name_v, fcpl_id_v, fapl_id_v, flags_v);

  const char* name = String_val(name_v);
  unsigned flags = acc_val(flags_v);
  hid_t fcpl_id = H5P_opt_val(fcpl_id_v), fapl_id = H5P_opt_val(fapl_id_v);

  CAMLreturn(alloc_h5f(H5Fcreate(name, flags, fcpl_id, fapl_id)));
}

#include <caml/custom.h>
#include <caml/fail.h>
#include "hdf5.h"
#include "h5i_stubs.h"
#include "h5l_stubs.h"

H5L_type_t H5L_type_val(value type)
{
  switch (Int_val(type))
  {
    case  0: return H5L_TYPE_HARD;
    case  1: return H5L_TYPE_SOFT;
    case  2: return H5L_TYPE_EXTERNAL;
    case  3: return H5L_TYPE_MAX;
    default: caml_failwith("unrecognized H5L_type_t");
  }
}

value Val_h5l_type(H5L_type_t layout)
{
  switch (layout)
  {
    case H5L_TYPE_ERROR:    fail();
    case H5L_TYPE_HARD:     return Val_int(0);
    case H5L_TYPE_SOFT:     return Val_int(1);
    case H5L_TYPE_EXTERNAL: return Val_int(2);
    case H5L_TYPE_MAX:      return Val_int(3);
    default: caml_failwith("unrecognized H5L_type_t");
  }
}

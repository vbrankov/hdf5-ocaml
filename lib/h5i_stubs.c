#include <caml/callback.h>
#include <caml/fail.h>
#include "hdf5.h"

void fail()
{
  caml_raise_constant(*caml_named_value("HDF5.H5I.Fail"));
}

void raise_if_fail(hid_t id)
{
  if (id == -1)
    fail();
}

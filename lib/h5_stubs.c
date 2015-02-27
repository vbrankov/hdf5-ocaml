#include <stddef.h>
#include <stdlib.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include "hdf5.h"

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

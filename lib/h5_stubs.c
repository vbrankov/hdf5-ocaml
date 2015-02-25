#include <stddef.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include "hdf5.h"

int hsize_t_array_val(value v, hsize_t **a)
{
  int i, length;

  length = Wosize_val(v);
  *a = calloc(length, sizeof(hsize_t));
  if (*a == NULL)
    return length;
  for (i = 0; i < length; i++)
    (*a)[i] = Field(v, i);
  return length;
}

int hsize_t_array_opt_val(value v, hsize_t **a)
{
  if (Is_block(v))
    return hsize_t_array_val(Field(v, 0), a);
  (*a) = NULL;
  return 0;
}

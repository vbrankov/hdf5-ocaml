#include <stdbool.h>

#define H5D_val(v) *((hid_t*) Data_custom_val(v))
#define H5D_closed(v) *((bool*) ((char*) Data_custom_val(v) + sizeof(hid_t)))
H5D_layout_t H5D_layout_val(value);
value Val_h5d_layout(H5D_layout_t);

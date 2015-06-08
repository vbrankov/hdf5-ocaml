#include <stdbool.h>

#define H5S_val(v) *((hid_t*) Data_custom_val(v))
#define H5S_opt_val(v) Is_block(v) ? H5S_val(Field(v, 0)) : -1
#define H5S_closed(v) *((bool*) ((char*) Data_custom_val(v) + sizeof(hid_t)))
value alloc_h5s(hid_t id);

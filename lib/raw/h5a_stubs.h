#include <stdbool.h>

#define H5A_val(v) *((hid_t*) Data_custom_val(v))
#define H5A_closed(v) *((bool*) ((char*) Data_custom_val(v) + sizeof(hid_t)))

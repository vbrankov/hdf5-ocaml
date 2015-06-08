#define H5G_val(v) *((hid_t*) Data_custom_val(v))
#define H5G_closed(v) *((bool*) ((char*) Data_custom_val(v) + sizeof(hid_t)))

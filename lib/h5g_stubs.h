#define H5G_val(v) *((hid_t*) Data_custom_val(v))
#define Loc_val(v) H5G_val(Field(v, 1))

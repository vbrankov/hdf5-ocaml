#define H5P_val(v) *((hid_t*) Data_custom_val(v))
#define H5P_opt_val(v) Is_block(v) ? H5P_val(Field(v, 0)) : H5P_DEFAULT
value alloc_h5p(hid_t id);

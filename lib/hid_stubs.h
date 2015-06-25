#define Hid_val(v) *((hid_t*) Data_custom_val(v))
value alloc_hid(hid_t);
value val_hid_array(int, hid_t*);

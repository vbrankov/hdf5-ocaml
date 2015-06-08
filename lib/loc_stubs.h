#define Loc_val(v) *((hid_t*) Data_custom_val(v))
value alloc_loc(hid_t);
value val_loc_array(int, hid_t*);

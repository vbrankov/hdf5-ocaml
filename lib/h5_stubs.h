#define Int_opt_val(v, d) Is_block(v) ? Int_val(Field(v, 0)) : d

int hsize_t_array_val(value v, hsize_t **a);
int hsize_t_array_opt_val(value v, hsize_t **a);
value val_hsize_t_array(int length, hsize_t *a);
H5_iter_order_t H5_iter_order_val(value);
H5_iter_order_t H5_iter_order_opt_val(value v);
value Val_h5_iter_order(H5_iter_order_t);
int H5_iter_val(value);
value Val_h5_iter(int);
H5_index_t H5_index_val(value);
H5_index_t H5_index_opt_val(value v);
value Val_h5_index(H5_index_t);
H5_ih_info_t H5_ih_info_val(value);
value Val_h5_ih_info(H5_ih_info_t);

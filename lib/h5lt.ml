open Bigarray

external make_dataset : Loc.t -> string -> Hsize.t array -> H5t.t -> _ Genarray.t -> unit
  = "hdf5_h5lt_make_dataset"

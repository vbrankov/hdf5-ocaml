open Hdf5_raw

(* This is an interface to HDF5 tables, which have memory representation as C arrays of
   structs.  [Mem.t] represents an array of structs and [Ptr.t] represents a pointer to an
   element of such array.  Both are implemented as custom blocks to support marshalling.
   Since GC does not examine custom blocks, memory management of the tables is done using
   reference counting.  Both [Mem.t] and [Ptr.t] point to a C struct [Mem.T.t] which is
   allocated outside of the OCaml heap and which contains the reference count.  [Mem.T.t]
   also contains [nmemb] and [size] which describe the size and the shape of the table.

   Marshalling supports sharing. *)

module type S = sig
  val fields : Field.t list
end

(* Pointer outside of the OCaml heap. *)
module Ext = struct
  type t = private int

  (* [badd t i] adds [2 * i] bytes offset to the pointer [t]. *)
  let badd (t : t) i : t = Obj.magic (Obj.magic t + i)
  (* [badd t i] subtracts [2 * i] bytes offset from the pointer [t]. *)
  let bsub (t : t) i : t = Obj.magic (Obj.magic t - i)

  (* [get_float64 t boffset] returns a float stored [2 * boffset] bytes from [t]. *)
  let get_float64 (t : t) boffset =
    Array.unsafe_get (Obj.magic (badd t boffset) : float array) 0

  (* [set_float64 t boffset v] sets a float [2 * boffset] bytes from [t]. *)
  let set_float64 (t : t) boffset v =
    Array.unsafe_set (Obj.magic (badd t boffset) : float array) 0 v

  external caml_string_get_64 : t -> int -> int64         = "%caml_string_get64u"
  external caml_string_set_64 : t -> int -> int64 -> unit = "%caml_string_set64u"

  (* [get_int64 t boffset] returns an int64 stored [2 * boffset] bytes from [t]. *)
  let get_int64 (t : t) boffset   = caml_string_get_64 (badd t boffset) 0

  (* [set_int64 t boffset v] sets an int64 [2 * boffset] bytes from [t]. *)
  let set_int64 (t : t) boffset v = caml_string_set_64 (badd t boffset) 0 v

  (* [get_int t boffset] returns an int stored [2 * boffset] bytes from [t]. *)
  let get_int t boffset   = Int64.to_int (get_int64 t boffset)

  (* [set_int t boffset v] sets an int [2 * boffset] bytes from [t]. *)
  let set_int t boffset v = set_int64 t boffset (Int64.of_int v)

  (* [get_string t boffset] returns a string stored [2 * boffset] bytes from [t]. *)
  let get_string (t : t) pos len =
    let rec index t c l len =
      if l >= len then len
      else if String.unsafe_get (Obj.magic t) l = c then l
      else index t c (l + 1) len
    in
    let t = badd t pos in
    let len = index t '\000' 0 len in
    let s = Bytes.create len in
    Bytes.unsafe_blit (Obj.magic t) 0 s 0 len;
    Bytes.unsafe_to_string s

  (* [set_string t boffset v] sets a string [2 * boffset] bytes from [t]. *)
  let set_string (t : t) pos len v =
    let t = badd t pos in
    let vlen = String.length v in
    let mlen = if len < vlen then len else vlen in
    Bytes.unsafe_blit (Bytes.unsafe_of_string v) 0 (Obj.magic t) 0 mlen;
    Bytes.unsafe_fill (Obj.magic t) mlen (len - mlen) '\000'
end

(* See the explanation at the top. *)
module Mem = struct
  (* This is a C structure living outside of the OCaml heap.  This is not an OCaml record
     so nothing other than [Mem.t] and [Ptr.t] should keep a reference to it.  It is
     implemented this was to allow quick access to the fields, otherwise we would have to
     use relatively slow C calls to read each field. *)
  module T = struct
    type t = {
      refcount      : int;
      data          : Ext.t;
      capacity      : int; (* The capacity of [data] *)
      mutable nmemb : int; (* The number of records in the table *)
      size          : int; (* The length of a record *)
    }
  end

  type t = {
    ops : Ext.t; (* Custom operations field *)
    t   : T.t;
  }

  external create : int -> int -> t = "hdf5_caml_struct_mem_create"

  external of_t : T.t -> t = "hdf5_caml_struct_mem_of_mem"

  external realloc : t -> int -> unit = "hdf5_caml_struct_mem_realloc"

  external blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
    = "hdf5_caml_struct_mem_blit"

  let data t : H5tb.Data.t = Obj.magic t.t.data
end

module Ptr = struct
  (* A pointer into an array of C structs wrapped in [Mem.t]. *)
  type t = {
    ops         : Ext.t;
    (* Pointer to the [pos]-th element of the array. *)
    mutable ptr : Ext.t;
    (* The underlying [Mem.t]. *)
    mutable mem : Mem.T.t;
    (* The index of the element of the array pointed by [ptr]. *)
    mutable pos : int;
  }

  external create : Mem.t -> int -> t = "hdf5_caml_struct_ptr_create"

  (* [unsafe_next t bsize] moves the pointer to the next element of the array provided
     that the length of the struct is [2 * bsize] bytes. *)
  let unsafe_next t bsize =
    t.ptr <- Ext.badd t.ptr bsize;
    t.pos <- t.pos + 1

  (* [unsafe_prev t bsize] moves the pointer to the previous element of the array provided
     that the length of the struct is [2 * bsize] bytes. *)
  let unsafe_prev t bsize =
    t.ptr <- Ext.bsub t.ptr bsize;
    t.pos <- t.pos - 1

  (* [unsafe_move t pos bsize] moves the pointer to the [pos]-th element of the array
     provided that the length of the struct is [2 * bsize] bytes. *)
  let unsafe_move t pos bsize =
    t.ptr <- Ext.badd t.mem.data (pos * bsize);
    t.pos <- pos

  (* Moves the pointer to the appropriate place in the array when [t.mem.data] changes. *)
  let reset t bsize = t.ptr <- Ext.badd t.mem.data (t.pos * bsize)

  (* Like [unsafe_next] but raises exception if the pointer is out of bounds. *)
  let next t bsize =
    let pos = t.pos + 1 in
    if pos >= t.mem.nmemb
    then raise (Invalid_argument "index out of bounds")
    else begin
      t.ptr <- Ext.badd t.ptr bsize;
      t.pos <- pos
    end

  (* Like [unsafe_prev] but raises exception if the pointer is out of bounds. *)
  let prev t bsize =
    let pos = t.pos - 1 in
    if pos < 0
    then raise (Invalid_argument "index out of bounds")
    else begin
      t.ptr <- Ext.bsub t.ptr bsize;
      t.pos <- pos
    end

  (* Like [unsafe_move] but raises exception if the pointer is out of bounds. *)
  let move t pos bsize =
    if pos < 0 || pos >= t.mem.nmemb
    then raise (Invalid_argument "index out of bounds")
    else begin
      t.ptr <- Ext.badd t.mem.data (pos * bsize);
      t.pos <- pos
    end

  let get_float64 t bo       = Ext.get_float64 t.ptr bo
  let set_float64 t bo v     = Ext.set_float64 t.ptr bo v
  let get_int64   t bo       = Ext.get_int64   t.ptr bo
  let set_int64   t bo v     = Ext.set_int64   t.ptr bo v
  let get_int     t bo       = Ext.get_int     t.ptr bo
  let set_int     t bo v     = Ext.set_int     t.ptr bo v
  let get_string  t bo len   = Ext.get_string  t.ptr bo len
  let set_string  t bo len v = Ext.set_string  t.ptr bo len v

  let seek_float64 t bsize bfield ~min ~max v =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = Ext.badd t.mem.data bfield in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      let v' = Ext.get_float64 data (!mid * bsize) in
      if v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Ext.get_float64 data (!max * bsize) in
    if v' <= v then !max else !min

  (* [seek_float64 t bsize bfield v] seeks the last element of the array with the value of
     the given field less or equal [v].  The field is [2 * bfield] bytes from the
     beginning of the struct.  The length of the struct is [2 * bsize] bytes.  The array
     elements are sorted increasingly by the given field. *)
  let seek_float64 t bsize bfield v =
    let len = t.mem.nmemb in
    let data = Ext.badd t.mem.data bfield in
    let v' = Ext.get_float64 t.ptr bfield in
    let pos = t.pos in
    let min = ref pos in
    let max = ref pos in
    let step = ref 1 in
    if v' < v then begin
      while !max < len
        && Ext.get_float64 data (!max * bsize) < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      while !min > 0 && Ext.get_float64 data (!min * bsize) > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t
      (if !max > !min then seek_float64 t bsize bfield ~min:!min ~max:!max v else !max)
      bsize

  let seek_int t bsize bfield ~min ~max v =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = Ext.badd t.mem.data bfield in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      let v' = Ext.get_int data (!mid * bsize) in
      if v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Ext.get_int data (!max * bsize) in
    if v' <= v then !max else !min

  (* [seek_int t bsize bfield v] seeks the last element of the array with the value of
     the given field less or equal [v].  The field is [2 * bfield] bytes from the
     beginning of the struct.  The length of the struct is [2 * bsize] bytes.  The array
     elements are sorted increasingly by the given field. *)
  let seek_int t bsize bfield v =
    let len = t.mem.nmemb in
    let data = Ext.badd t.mem.data bfield in
    let v' = Ext.get_int t.ptr bfield in
    let pos = t.pos in
    let min = ref pos in
    let max = ref pos in
    let step = ref 1 in
    if v' < v then begin
      while !max < len && Ext.get_int data (!max * bsize) < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      while !min > 0 && Ext.get_int data (!max * bsize) > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_int t bsize bfield ~min:!min ~max:!max v else !max) bsize

  let seek_int64 t bsize bfield ~min ~max (v : int64) =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = Ext.badd t.mem.data bfield in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      let v' = Ext.get_int64 data (!mid * bsize) in
      if v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Ext.get_int64 data (!max * bsize) in
    if v' <= v then !max else !min

  (* [seek_int64 t bsize bfield v] seeks the last element of the array with the value of
     the given field less or equal [v].  The field is [2 * bfield] bytes from the
     beginning of the struct.  The length of the struct is [2 * bsize] bytes.  The array
     elements are sorted increasingly by the given field. *)
  let seek_int64 t bsize bfield v =
    let len = t.mem.nmemb in
    let data = Ext.badd t.mem.data bfield in
    let v' = Ext.get_int64 t.ptr bfield in
    let pos = t.pos in
    let min = ref pos in
    let max = ref pos in
    let step = ref 1 in
    if v' < v then begin
      while !max < len && Ext.get_int64 data (!max * bsize) < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      while !min > 0 && Ext.get_int64 data (!min * bsize) > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t
      (if !max > !min then seek_int64 t bsize bfield ~min:!min ~max:!max v else !max)
      bsize

  let seek_string t bsize bfield len ~min ~max (v : string) =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = Ext.badd t.mem.data bfield in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      let v' = Ext.get_string data (!mid * bsize) len in
      if v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Ext.get_string data (!max * bsize) len in
    if v' <= v then !max else !min

  (* [seek_string t bsize bfield len v] seeks the last element of the array with the value
     of the given field less or equal [v].  The field is [2 * bfield] bytes from the
     beginning of the struct.  The length of the struct is [2 * bsize] bytes.  The array
     elements are sorted increasingly by the given field. *)
  let seek_string t bsize bfield slen v =
    let len = t.mem.nmemb in
    let data = Ext.badd t.mem.data bfield in
    let v' = Ext.get_string t.ptr bfield slen in
    let pos = t.pos in
    let min = ref pos in
    let max = ref pos in
    let step = ref 1 in
    if v' < v then begin
      while !max < len && Ext.get_string data (!max * bsize) slen < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      while !min > 0 && Ext.get_string data (!min * bsize) slen > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_string t bsize bfield slen ~min:!min ~max:!max v else !max)
      bsize
end

module Make(S : S) = struct
  include S

  (* This is necessary to prevent external pointers from being top level values of the
     module, which would prevent it from being marshaled *)
  let memoize f =
    let memo = ref None in
    fun () ->
      match !memo with
      | Some value -> value
      | None ->
        let value = f () in
        memo := Some value;
        value

  let nfields = List.length S.fields
  let type_bsize = List.fold_left (fun s field ->
    s + (Type.size field.Field.type_ + 7) / 8 * 8 / 2) 0 S.fields
  let type_size = 2 * type_bsize
  let field_names =
    List.map (fun field -> field.Field.name) S.fields
    |> Array.of_list
  let field_offset =
    let offset = ref 0 in
    List.map (fun field ->
      let field_offset = !offset in
      offset := !offset + (Type.size field.Field.type_ + 7) / 8 * 8;
      field_offset) S.fields
    |> Array.of_list
  let field_types = memoize (fun () ->
    List.map (fun field ->
      match field.Field.type_ with
      | Type.Int | Type.Int64 -> H5t.native_long
      | Type.Float64 -> H5t.native_double
      | Type.String l ->
        let type_ = H5t.copy H5t.c_s1 in
        H5t.set_size type_ l;
        type_) S.fields
    |> Array.of_list)
  let field_sizes =
    List.map (fun field -> Type.size field.Field.type_) S.fields
    |> Array.of_list
  let compound_type = memoize (fun () ->
    let datatype = H5t.create H5t.Class.COMPOUND type_size in
    let field_types = field_types () in
    for i = 0 to nfields - 1 do
      H5t.insert datatype field_names.(i) field_offset.(i) field_types.(i)
    done;
    datatype)

  include Ptr

  let pos t = t.Ptr.pos
  let has_next (t : t) = t.pos + 1 < t.mem.nmemb
  let has_prev t = t.Ptr.pos > 0
  let unsafe_next t = Ptr.unsafe_next t type_bsize
  let next t = Ptr.next t type_bsize
  let move t i = Ptr.move t i type_bsize
  let unsafe_move t i = Ptr.unsafe_move t i type_bsize

  module Array = struct
    type e = t
    type t = Mem.t

    let make len = Mem.create len type_size

    let length (t : t) = t.t.nmemb

    let unsafe_get (t : t) pos = Ptr.create t pos

    let init len f =
      if len < 0 then invalid_arg "Hdf5_caml.Struct.Array.init";
      let t = make len in
      if len = 0 then t
      else begin
        let e = unsafe_get t 0 in
        for i = 0 to len - 2 do
          f i e;
          unsafe_next e
        done;
        f (len - 1) e;
        t
      end

    let get (t : t) pos =
      if pos < 0 || pos >= t.t.nmemb then raise (Invalid_argument "index out of bounds");
      Ptr.create t pos

    let make_table t ?title ?chunk_size ?(compress = true) h5
        dset_name =
      let title = match title with Some t -> t | None -> dset_name in
      let chunk_size =
        match chunk_size with
        | Some s -> s
        (* Chunk size must be <4GB *)
        | None -> min (1024 * 1024) (max 1 (length t)) in
      H5tb.make_table title (H5.hid h5) (H5.escape dset_name) ~nrecords:(length t)
        ~type_size ~field_names ~field_offset ~field_types:(field_types ()) ~chunk_size
        ~compress (Mem.data t)

    let append_records t h5 dset_name =
      H5tb.append_records (H5.hid h5) (H5.escape dset_name) ~nrecords:(length t)
        ~type_size ~field_offset ~field_sizes (Mem.data t)

    let write_records t h5 ~start dset_name =
      H5tb.write_records (H5.hid h5) (H5.escape dset_name) ~start ~nrecords:(length t)
        ~type_size ~field_offset ~field_sizes (Mem.data t)

    let read_table h5 table_name =
      let table_name = H5.escape table_name in
      let loc = H5.hid h5 in
      let nrecords = H5tb.get_table_info loc table_name in
      let t = make nrecords in
      H5tb.read_table loc table_name ~dst_size:type_size ~dst_offset:field_offset
        ~dst_sizes:field_sizes (Mem.data t);
      t

    let read_records h5 ~start ~nrecords table_name =
      let loc = H5.hid h5 in
      let t = make nrecords in
      H5tb.read_records loc (H5.escape table_name) ~start ~nrecords ~type_size
        ~field_offset ~dst_sizes:field_sizes (Mem.data t);
      t

    let write t ?(deflate = H5.default_deflate ()) h5 name =
      let len = length t in
      let dims = [| len |] in
      let dataspace = H5s.create_simple dims in
      (* Chunk size must be <4GB *)
      dims.(0) <- min (1024 * 1024) dims.(0);
      let dcpl =
        match deflate with
        | 0 -> None
        | _ when len = 0 -> None
        | deflate ->
          let dcpl = H5p.create H5p.Cls_id.DATASET_CREATE in
          H5p.set_chunk dcpl dims;
          H5p.set_deflate dcpl deflate;
          Some dcpl
      in
      let compound_type = compound_type () in
      let dataset =
        H5d.create (H5.hid h5) (H5.escape name) compound_type ?dcpl dataspace in
      H5d.write_string dataset compound_type H5s.all H5s.all (Mem.data t |> Obj.magic);
      H5d.close dataset;
      H5s.close dataspace;
      match dcpl with
      | None -> ()
      | Some dcpl -> H5p.close dcpl

    let read h5 ?data name =
      let hid = H5.hid h5 in
      let dataset = H5d.open_ hid (H5.escape name) in
      let datatype = H5d.get_type dataset in
      let compound_type = compound_type () in
      if not (H5t.equal compound_type datatype) then
        invalid_arg "Unexpected datatype";
      let dataspace = H5d.get_space dataset in
      let dims, _ = H5s.get_simple_extent_dims dataspace in
      if Array.length dims <> 1 then invalid_arg "Dataset not one dimensional";
      let data =
        match data with
        | Some data ->
          if length data < dims.(0) then
            invalid_arg "The provided data storage too small";
          data
        | None -> make dims.(0) in
      H5d.read_string dataset datatype H5s.all H5s.all (Mem.data data |> Obj.magic);
      H5s.close dataspace;
      H5t.close datatype;
      H5d.close dataset;
      data

    let iter t ~f =
      let e = unsafe_get t 0 in
      for _ = 0 to length t - 1 do
        f e;
        unsafe_next e
      done

    let iteri t ~f =
      let e = unsafe_get t 0 in
      for i = 0 to length t - 1 do
        f i e;
        unsafe_next e
      done

    let data t = Mem.data t
  end

  let create () = Array.unsafe_get (Array.make 1) 0
  let mem t = Mem.of_t t.mem

  module Vector = struct
    type e = t
    type t = {
      mem                   : Mem.t;
      mutable growth_factor : float;
      mutable end_          : e;
      mutable ptrs          : e list;
      mutable on_realloc    : t -> unit;
    }

    let create ?(capacity = 16) ?(growth_factor = 1.5) () =
      if growth_factor < 1. then
        invalid_arg (Printf.sprintf "Invalid growth factor %f" growth_factor);
      let mem = Array.make capacity in
      mem.t.nmemb <- 0;
      { mem; growth_factor; end_ = Array.unsafe_get mem (-1);
        ptrs = []; on_realloc = fun _ -> () }

    let capacity t = t.mem.t.capacity

    let growth_factor t = t.growth_factor

    let set_growth_factor t growth_factor =
      if growth_factor <= 0. then
        invalid_arg (Printf.sprintf "Given negative growth factor %g" growth_factor);
      t.growth_factor <- growth_factor

    let length t = t.mem.t.nmemb

    let set_length t length = t.mem.t.nmemb <- length

    let end_ t =
      if length t <= 0 then raise (Invalid_argument "index out of bounds");
      t.end_

    let realloc t capacity =
      Mem.realloc t.mem capacity;
      List.iter (fun ptr -> Ptr.reset ptr type_bsize) t.ptrs;
      Ptr.reset t.end_ type_bsize;
      t.on_realloc t

    let ensure_capacity t c = if c > capacity t then realloc t c

    let append t =
      let mem = t.mem.t in
      let nmemb = mem.nmemb in
      let capacity = mem.capacity in
      if capacity = nmemb then
        realloc t (int_of_float (float capacity *. t.growth_factor) + 1);
      mem.nmemb <- nmemb + 1;
      next t.end_;
      t.end_

    let clear t =
      set_length t 0;
      unsafe_move t.end_ (-1)

    let unsafe_get t i =
      let e = Array.unsafe_get t.mem i in
      t.ptrs <- e :: t.ptrs;
      e

    let get t i =
      if i < 0 || i >= length t then raise (Invalid_argument "index out of bounds");
      let e = Array.get t.mem i in
      t.ptrs <- e :: t.ptrs;
      e

    let iter t ~f =
      let ptr = t.end_ in
      unsafe_move ptr 0;
      let len = length t in
      for _ = 0 to len - 1 do
        f ptr;
        unsafe_next ptr
      done;
      unsafe_move ptr (len - 1)

    let iteri t ~f =
      let ptr = t.end_ in
      unsafe_move ptr 0;
      let len = length t in
      for i = 0 to len - 1 do
        f i ptr;
        unsafe_next ptr
      done;
      unsafe_move ptr (len - 1)

    let of_array ?(growth_factor = 1.5) a =
      if growth_factor < 1. then
        invalid_arg (Printf.sprintf "Invalid growth factor %f" growth_factor);
      let len = Array.length a in
      { mem = a;
        growth_factor;
        end_ = Array.unsafe_get a (if len > 0 then len - 1 else 0);
        ptrs = [];
        on_realloc = fun _ -> () }

    let to_array t =
      let len = length t in
      let mem = Array.make len in
      Mem.blit ~src:t.mem ~src_pos:0 ~dst:mem ~dst_pos:0 ~len;
      mem

    let on_realloc t f = t.on_realloc <- f
  end

  module Queue = struct
    type e = t
    type t = {
      mutable a    : Array.t;
      mutable hd   : e;
      mutable tl   : e;
      mutable peek : e;
    }

    let create ?(capacity = 16) () =
      if capacity <= 0 then
        invalid_arg (Printf.sprintf "The given capacity %d cannot be negative" capacity);
      let a = Array.make capacity in
      { a; hd = Array.get a 0; tl = Array.get a 0; peek = Array.get a 0 }

    let next a e =
      let capacity = Array.length a in
      let pos = pos e in
      let new_pos = pos + 1 in
      if new_pos = capacity then unsafe_move e 0
      else unsafe_next e

    let is_empty t = pos t.hd = pos t.tl

    let length t =
      let l = pos t.hd - pos t.tl in
      if l >= 0 then l
      else l + Array.length t.a

    let add t =
      let { a; hd; tl; _ } = t in
      next a hd;
      if pos hd <> pos tl then hd
      else begin
        let capacity = Array.length a in
        let new_capacity = 1 + capacity * 3 / 2 in
        t.a    <- Array.make new_capacity;
        t.hd   <- Array.get t.a 0;
        t.tl   <- Array.get t.a 0;
        t.peek <- Array.get t.a 0;
        let pos = pos hd in
        if pos = 0 then
          Mem.blit ~src:a ~src_pos:0 ~dst:t.a ~dst_pos:0 ~len:capacity
        else begin
          Mem.blit ~src:a ~src_pos:pos ~dst:t.a ~dst_pos:0 ~len:(capacity - pos);
          Mem.blit ~src:a ~src_pos:0   ~dst:t.a ~dst_pos:(capacity - pos) ~len:pos
        end;
        unsafe_move t.hd capacity;
        t.hd
      end

    let take { a; hd; tl; _ } =
      if pos hd = pos tl then raise Queue.Empty;
      next a tl;
      tl

    let peek { a; hd; tl; peek } =
      if pos hd = pos tl then raise Queue.Empty;
      unsafe_move peek (pos tl);
      next a peek;
      peek
  end
end

external reset_serialize : unit -> unit = "hdf5_caml_struct_reset_serialize"
external reset_deserialize : unit -> unit = "hdf5_caml_struct_reset_deserialize"

let%test_module "" = (module struct
  module Foo = struct
    include Make(struct
      let fields = [
        Field.create "id" Int;
        Field.create "name" (String 10);
      ]
    end)

    let id t = get_int t 0
    let name t = get_string t 4 10

    let set t ~id ~name =
      set_int    t 0 id;
      set_string t 4 10 name
  end

  let%test_unit _ =
    let v = Foo.Vector.create () in
    let _ = Foo.Vector.append v in
    let f = Foo.Vector.get v 0 in
    for i = 0 to 999 do
      let s = string_of_int i in
      let e = Foo.Vector.append v in
      Foo.set e ~id:i ~name:s;
      Foo.next f;
      assert (Foo.id f = i);
      assert (Foo.name f = s);
    done;
    let a = Foo.Vector.to_array v in
    let f = Foo.Array.get a 0 in
    assert (not (Foo.has_prev f));
    assert (Foo.has_next f);
    Foo.next f;
    assert (Foo.has_prev f);
    assert (Foo.has_next f);
    Foo.move f 1000;
    assert (Foo.has_prev f);
    assert (not (Foo.has_next f))

  let%test_unit _ =
    let len = 32 in
    let create_array () =
      Foo.Array.init (1 + Random.int len) (fun i e ->
        Foo.set e ~id:i ~name:(string_of_int i)) in
    let a = ref (Array.init len (fun _ -> create_array ())) in
    let create_element () =
      let a = !a.(Random.int len) in
      let pos = Random.int (Foo.Array.length a) in
      pos, Foo.Array.get a pos in
    let e = ref (Array.init (len * len) (fun _ -> create_element ())) in
    for _ = 0 to len - 1 do
      for _ = 0 to len - 1 do
        let a = !a in
        let e = !e in
        for _ = 0 to len - 1 do
          a.(Random.int len) <- create_array ();
          for _ = 0 to len - 1 do
            e.(Random.int (len * len)) <- create_element ();
            let pos, e = e.(Random.int (len * len)) in
            assert (Foo.id e = pos);
            assert (Foo.name e = string_of_int pos)
          done
        done;
        Gc.full_major ()
      done;
      Gc.compact ();
      reset_serialize ();
      let s = Marshal.to_string (!a, !e) [] in
      reset_deserialize ();
      let a', e' = Marshal.from_string s 0 in
      a := a';
      e := e'
    done

  let%test_unit _ =
    begin try
      let _ = Foo.Array.init (-1) (fun _ _ -> ()) in
      assert false
    with Invalid_argument _ -> ()
    end

  let%test_unit _ =
    let a = Foo.Array.init 16 (fun i e -> Foo.set e ~id:i ~name:(string_of_int i)) in

    reset_serialize ();
    let s = Marshal.to_string a [Closures] in
    reset_deserialize ();
    let b = Marshal.from_string s 0 in
    assert (Foo.Array.length a = Foo.Array.length b);
    let f = Foo.Array.get b 0 in
    Foo.Array.iteri a ~f:(fun i e ->
      Foo.move f i;
      assert (Foo.id e = Foo.id f);
      assert (Foo.name e = Foo.name f));

    let e = Foo.Array.get a 8 in
    reset_serialize ();
    let s = Marshal.to_string e [Closures] in
    reset_deserialize ();
    let e = Marshal.from_string s 0 in
    assert (Foo.id e = 8);
    assert (Foo.name e = "8")
end)

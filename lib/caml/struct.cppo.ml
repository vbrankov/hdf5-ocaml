open Bigarray
open Hdf5_raw

module type S = sig
  val fields : Field.t list
end

module Mem = struct
  module Array1 = struct
    type t = (char, int8_unsigned_elt, c_layout) Array1.t
  end

  type t = {
    ops      : int;
    data     : int;
    num_dims : int;
    flags    : int;
    proxy    : int;
    dim      : int;
  }

  let to_array1 (t : t) : Array1.t = Obj.magic t
  let of_array1 (t : Array1.t) : t = Obj.magic t
end

module Ptr = struct
  type t = {
    mutable ptr    : int;
    mutable mem    : Mem.t;
    mutable begin_ : int;
    mutable end_   : int;
    mutable len    : int;
    mutable i      : int;
  }

  let unsafe_next t size =
    let ptr = t.ptr + size in
    t.ptr <- ptr;
    t.i <- t.i + 1

  let unsafe_prev t size =
    let ptr = t.ptr - size in
    t.ptr <- ptr;
    t.i <- t.i - 1

  let unsafe_move t i size =
    t.ptr <- t.begin_ + i * size;
    t.i <- i

  let next t size =
    let ptr = t.ptr + size in
    if ptr > t.end_
    then raise (Invalid_argument "index out of bounds")
    else begin
      t.ptr <- ptr;
      t.i <- t.i + 1
    end

  let prev t size =
    let ptr = t.ptr - size in
    if ptr < t.begin_
    then raise (Invalid_argument "index out of bounds")
    else begin
      t.ptr <- ptr;
      t.i <- t.i - 1
    end

  let move t i size =
    let ptr = t.begin_ + i * size in
    if i < 0 || ptr > t.end_
    then raise (Invalid_argument "index out of bounds")
    else begin
      t.ptr <- ptr;
      t.i <- i
    end

  let get_float64 t i   =
    Obj.magic (Array.unsafe_get (Obj.magic t.ptr : float array) i)
  let set_float64 t i v =
    Array.unsafe_set (Obj.magic t.ptr : float array) i (Obj.magic v)
  let get_int t i = Obj.magic (
    Int64.to_int (Obj.magic (Obj.magic t.ptr + (i - 1) * 4) : int64))
  let set_int t i v =
    let a : (int64, int64_elt, c_layout) Array1.t = Obj.magic (Obj.magic t - 4) in
    Array1.unsafe_set a i (Int64.of_int (Obj.magic v))
  let get_int64 t i =
    let a : (int64, int64_elt, c_layout) Array1.t = Obj.magic (Obj.magic t - 4) in
    Obj.magic (Array1.unsafe_get a i)
  let set_int64 t i v =
    let a : (int64, int64_elt, c_layout) Array1.t = Obj.magic (Obj.magic t - 4) in
    Array1.unsafe_set a i (Obj.magic v)

  external unsafe_fill : bytes -> int -> int -> char -> unit
#if OCAML_VERSION >= (4, 2, 0)
                       = "caml_fill_string" [@@noalloc]
#else
                       = "caml_fill_string" "noalloc"
#endif
  external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
#if OCAML_VERSION >= (4, 2, 0)
                       = "caml_blit_string" [@@noalloc]
#else
                       = "caml_blit_string" "noalloc"
#endif

  let get_string =
    let rec index ptr c pos l len =
      if l >= len then len
      else if String.unsafe_get ptr pos = c then l
      else index ptr c (pos + 1) (l + 1) len
    in
    fun t pos len ->
      let t = Obj.magic t in
      let len = index (Obj.magic t.ptr) '\000' pos 0 len in
      let s = Bytes.create len in
      unsafe_blit_string (Obj.magic t.ptr) pos s 0 len;
      s

  let set_string t pos len v =
    let t = Obj.magic t in
    let vlen = String.length v in
    let mlen = if len < vlen then len else vlen in
    unsafe_blit_string v 0 (Obj.magic t.ptr) pos mlen;
    unsafe_fill (Obj.magic t.ptr) (pos + mlen) (len - mlen) '\000'

  let seek_float64 t size pos ~min ~max v =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = t.mem.Mem.data + pos * 4 in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      let v' = Array.unsafe_get (Obj.magic (data + !mid * size) : float array) 0 in
      if v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Array.unsafe_get (Obj.magic (data + !max * size) : float array) 0 in
    if v' <= v then !max else !min

  let seek_float64 t size pos v =
    let v : float = Obj.magic v in
    let t = Obj.magic t in
    let data = t.mem.Mem.data in
    if t.len < 0 then t.len <- t.mem.Mem.dim / size;
    let len = t.len in
    if data + t.i * size <> t.ptr then
      t.i <- (t.ptr - data) / size;
    let i = t.i in
    let pos4 = pos * 4 in
    let data = data + pos4 in
    let v' = Array.unsafe_get (Obj.magic (t.ptr + pos4) : float array) 0 in
    let min = ref i in
    let max = ref i in
    let step = ref 1 in
    if v' < v then begin
      while !max < len
        && Array.unsafe_get (Obj.magic (data + !max * size)) 0 < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      while !min > 0 && Array.unsafe_get (Obj.magic (data + !min * size)) 0 > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_float64 t size pos ~min:!min ~max:!max v else !max) size

  let seek_int t size pos ~min ~max v =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = t.mem.Mem.data + (pos - 1) * 4 in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      let v' = Int64.to_int (Obj.magic (data + !mid * size)) in
      if v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Int64.to_int (Obj.magic (data + !max * size)) in
    if v' <= v then !max else !min

  let seek_int t size pos v =
    let v : int = Obj.magic v in
    let t = Obj.magic t in
    let data = t.mem.Mem.data in
    if t.len < 0 then t.len <- t.mem.Mem.dim / size;
    let len = t.len in
    if data + t.i * size <> t.ptr then
      t.i <- (t.ptr - data) / size;
    let i = t.i in
    let pos4 = pos * 4 - 4 in
    let data = data + pos4 in
    let v' = Int64.to_int (Obj.magic (t.ptr + pos4)) in
    let min = ref i in
    let max = ref i in
    let step = ref 1 in
    if v' < v then begin
      while !max < len && Int64.to_int (Obj.magic (data + !max * size)) < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      while !min > 0 && Int64.to_int (Obj.magic (data + !min * size)) > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_int t size pos ~min:!min ~max:!max v else !max) size

  let seek_int64 t size pos ~min ~max (v : int64) =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = t.mem.Mem.data + pos * 4 - 4 in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      let v' = Obj.magic (data + !mid * size) in
      if v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Obj.magic (data + !max * size) in
    if v' <= v then !max else !min

  let seek_int64 t size pos v =
    let v : int64 = Obj.magic v in
    let t = Obj.magic t in
    let data = t.mem.Mem.data in
    if t.len < 0 then t.len <- t.mem.Mem.dim / size;
    let len = t.len in
    if data + t.i * size <> t.ptr then
      t.i <- (t.ptr - data) / size;
    let i = t.i in
    let data = data + pos * 4 - 4 in
    let v' = Obj.magic (data + i * size) in
    let min = ref i in
    let max = ref i in
    let step = ref 1 in
    if v' < v then begin
      while !max < len && Obj.magic (data + !max * size) < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      while !min > 0 && Obj.magic (data + !min * size) > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_int64 t size pos ~min:!min ~max:!max v else !max) size

  let seek_string _t _size _pos _len _v =
    failwith "[seek_string] not implemented"
end

module Make(S : S) = struct
  include S

  let nfields = List.length S.fields
  let size64 = List.fold_left (fun s field ->
    s + (Type.size field.Field.type_ + 7) / 8 * 8 / 2) 0 S.fields
  let size = 2 * size64
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
  let field_types =
    let module H5t = Hdf5_raw.H5t in
    List.map (fun field ->
      match field.Field.type_ with
      | Type.Int | Type.Int64 -> H5t.native_long
      | Type.Float64 -> H5t.native_double
      | Type.String l ->
        let type_ = H5t.copy H5t.c_s1 in
        H5t.set_size type_ l;
        type_) S.fields
    |> Array.of_list
  let field_sizes =
    List.map (fun field -> Type.size field.Field.type_) S.fields
    |> Array.of_list
  let compound_type =
    let datatype = H5t.create H5t.Class.COMPOUND size in
    for i = 0 to nfields - 1 do
      H5t.insert datatype field_names.(i) field_offset.(i) field_types.(i)
    done;
    datatype

  include Ptr

  open Bigarray

  let pos t = t.Ptr.i
  let has_next t = t.Ptr.ptr + size64 < t.Ptr.end_
  let has_prev t = t.Ptr.i > 0
  let unsafe_next t = Ptr.unsafe_next t size64
  let next t = Ptr.next t size64
  let unsafe_move t i = Ptr.unsafe_move t i size64 

  module Array = struct
    type e = t
    type t = Mem.t

    let make len = Mem.of_array1 (Array1.create Char C_layout (len * size))

    let length t = t.Mem.dim / size64

    let unsafe_get t i =
      let data = t.Mem.data in
      { ptr = data + i * size64; mem = t; begin_ = data; end_ = data + t.Mem.dim;
        len = -1; i }

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

    let get t i =
      let ptr = t.Mem.data + i * size64 in
      let data = t.Mem.data in
      if i < 0 || ptr > data + t.Mem.dim then
        raise (Invalid_argument "index out of bounds");
      { ptr; mem = t; begin_ = data; end_ = data + t.Mem.dim; len = -1; i }

    module H5tb = Hdf5_raw.H5tb

    let make_table t ?title ?chunk_size ?(compress = true) h5
        dset_name =
      let title = match title with Some t -> t | None -> dset_name in
      let chunk_size = match chunk_size with Some s -> s | None -> length t in
      H5tb.make_table title (H5.hid h5) dset_name ~nrecords:(t.Mem.dim / size64)
        ~type_size:size ~field_names ~field_offset ~field_types ~chunk_size ~compress t

    let append_records t h5 dset_name =
      H5tb.append_records (H5.hid h5) dset_name ~nrecords:(t.Mem.dim / size64)
        ~type_size:size ~field_offset ~field_sizes t

    let write_records t h5 ~start dset_name =
      H5tb.write_records (H5.hid h5) dset_name ~start ~nrecords:(t.Mem.dim / size64)
        ~type_size:size ~field_offset ~field_sizes t

    let read_table h5 table_name =
      let loc = H5.hid h5 in
      let nrecords = H5tb.get_table_info loc table_name in
      let t = make nrecords in
      H5tb.read_table loc table_name ~dst_size:size ~dst_offset:field_offset
        ~dst_sizes:field_sizes t;
      t

    let read_records h5 ~start ~nrecords table_name =
      let loc = H5.hid h5 in
      let t = make nrecords in
      H5tb.read_records loc table_name ~start ~nrecords ~type_size:size ~field_offset
        ~dst_sizes:field_sizes t;
      t

    let write t ?(deflate = H5.default_deflate ()) h5 name =
      let open Hdf5_raw in
      let len = length t in
      let dims = [| len |] in
      let dataspace = H5s.create_simple dims in
      let dcpl =
        match deflate with
        | 0 -> None
        | deflate ->
          let dcpl = H5p.create H5p.Cls_id.DATASET_CREATE in
          H5p.set_chunk dcpl dims;
          H5p.set_deflate dcpl deflate;
          Some dcpl
      in
      let dataset = H5d.create (H5.hid h5) name compound_type ?dcpl dataspace in
      H5d.write dataset compound_type H5s.all H5s.all (Mem.to_array1 t);
      H5d.close dataset;
      H5s.close dataspace;
      match dcpl with
      | None -> ()
      | Some dcpl -> H5p.close dcpl

    let read h5 ?data name =
      let hid = H5.hid h5 in
      let dataset = H5d.open_ hid name in
      let datatype = H5d.get_type dataset in
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
      H5d.read dataset datatype H5s.all H5s.all data;
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
  end

  let create () = Array.unsafe_get (Array.make 1) 0
  let mem t = t.mem

  module Vector = struct
    type e = t
    type t = {
      mutable mem        : Mem.t;
      mutable capacity   : int;
      growth_factor      : float;
      mutable length     : int;
      mutable end_       : e;
      mutable ptrs       : e list;
      mutable on_realloc : t -> unit;
    }

    let create ?(capacity = 16) ?(growth_factor = 1.5) () =
      if growth_factor < 1. then
        invalid_arg (Printf.sprintf "Invalid growth factor %f" growth_factor);
      let mem = Array.make capacity in
      { mem; capacity; growth_factor; length = 0; end_ = Array.unsafe_get mem (-1);
        ptrs = []; on_realloc = fun _ -> () }

    let capacity t = t.capacity

    let growth_factor t = t.growth_factor

    let length t = t.length

    let realloc t capacity =
      if t.capacity > capacity then begin
        let mem = Array.make capacity in
        let dst = Mem.to_array1 mem in
        Array1.blit (Array1.sub (Mem.to_array1 t.mem) 0 (Array1.dim dst)) dst;
        t.mem <- mem
      end else if t.capacity < capacity then begin
        let mem = Array.make capacity in
        let src = Mem.to_array1 t.mem in
        Array1.blit src (Array1.sub (Mem.to_array1 mem) 0 (Array1.dim src));
        t.mem <- mem
      end;
      List.iter (fun ptr ->
        let ptr' = Array.get t.mem ptr.i in
        ptr.ptr    <- ptr'.ptr;
        ptr.mem    <- ptr'.mem;
        ptr.begin_ <- ptr'.begin_;
        ptr.end_   <- ptr'.end_;
        ptr.len    <- ptr'.len;
        ptr.i      <- ptr'.i) t.ptrs;
      t.end_ <- Array.unsafe_get t.mem (t.length - 1);
      t.capacity <- capacity;
      t.on_realloc t

    let set_length t length =
      t.length <- length;
      List.iter (fun ptr -> ptr.len <- length) t.ptrs;
      t.end_.len <- length

    let append t =
      if t.capacity = t.length then
        realloc t (int_of_float (float t.capacity *. t.growth_factor) + 1);
      set_length t (t.length + 1);
      next t.end_;
      t.end_

    let clear t =
      set_length t 0;
      unsafe_move t.end_ (-1)

    let unsafe_get t i =
      let e = Array.unsafe_get t.mem i in
      e.len <- t.length;
      t.ptrs <- e :: t.ptrs;
      e

    let get t i =
      let e = Array.get t.mem i in
      e.len <- t.length;
      t.ptrs <- e :: t.ptrs;
      e

    let iter t ~f =
      let ptr = t.end_ in
      unsafe_move ptr 0;
      for _ = 0 to t.length - 1 do
        f ptr;
        unsafe_next ptr
      done;
      unsafe_move ptr (t.length - 1)

    let iteri t ~f =
      let ptr = t.end_ in
      unsafe_move ptr 0;
      for i = 0 to t.length - 1 do
        f i ptr;
        unsafe_next ptr
      done;
      unsafe_move ptr (t.length - 1)

    let of_array ?(growth_factor = 1.5) a =
      if growth_factor < 1. then
        invalid_arg (Printf.sprintf "Invalid growth factor %f" growth_factor);
      let len = Array.length a in
      { mem = a; capacity = len; growth_factor; length = len;
        end_ = Array.get a (len - 1); ptrs = []; on_realloc = fun _ -> () }

    let to_array t =
      let mem = Array.make t.length in
      let dst = Mem.to_array1 mem in
      Array1.blit (Array1.sub (Mem.to_array1 t.mem) 0 (Array1.dim dst)) dst;
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
        let open Bigarray in
        let pos = pos hd in
        let size = size in
        let bpos = pos * size in
        let bcapacity = capacity * size in
        let a  : (char, int8_unsigned_elt, c_layout) Array1.t = Obj.magic a   in
        let ta : (char, int8_unsigned_elt, c_layout) Array1.t = Obj.magic t.a in
        if pos = 0 then
          Array1.blit a (Array1.sub ta 0 bcapacity)
        else begin
          Bigarray.Array1.blit
            (Array1.sub a  bpos (bcapacity - bpos))
            (Array1.sub ta 0    (bcapacity - bpos));
          Bigarray.Array1.blit
            (Array1.sub a  0                  bpos)
            (Array1.sub ta (bcapacity - bpos) bpos)
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

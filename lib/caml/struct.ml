module Type = struct
  type t =
  | Int
  | Int64
  | Float64
  | String of int

  let size = function
  | Int | Int64 | Float64 -> 8
  | String l -> l
end

module Field = struct
  type t = {
    name  : string;
    type_ : Type.t;
  }

  let create name type_ = { name; type_ }
end

module type S = sig
  val fields : Field.t list
end

module Mem = struct
  type t = {
    ops      : int;
    data     : int;
    num_dims : int;
    flags    : int;
    proxy    : int;
    dim      : int;
  }
end

module Ptr = struct
  type t = {
    mutable ptr : int;
    mem         : Mem.t;
    begin_      : int;
    end_        : int;
  }

  let unsafe_next t size =
    let t = Obj.magic t in
    let ptr = t.ptr + size in
    t.ptr <- ptr

  let unsafe_prev t size =
    let t = Obj.magic t in
    let ptr = t.ptr - size in
    t.ptr <- ptr

  let unsafe_move t i size =
    let t = Obj.magic t in
    t.ptr <- t.begin_ + i * size

  let next t size =
    let t = Obj.magic t in
    let ptr = t.ptr + size in
    if ptr > t.end_
    then raise (Invalid_argument "index out of bounds")
    else t.ptr <- ptr

  let prev t size =
    let t = Obj.magic t in
    let ptr = t.ptr - size in
    if ptr < t.begin_
    then raise (Invalid_argument "index out of bounds")
    else t.ptr <- ptr

  let move t i size =
    let t = Obj.magic t in
    let ptr = t.begin_ + i * size in
    if i < 0 || ptr > t.end_
    then raise (Invalid_argument "index out of bounds")
    else t.ptr <- ptr
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

  include Ptr

  external unsafe_fill : bytes -> int -> int -> char -> unit
                       = "caml_fill_string" "noalloc"
  external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
                       = "caml_blit_string" "noalloc"

  let get_string =
    let rec index ptr c pos l len =
      if l >= len then len
      else if String.unsafe_get ptr pos = c then l
      else index ptr c (pos + 1) (l + 1) len
    in
    fun t pos len ->
      let len = index (Obj.magic t.ptr) '\000' pos 0 len in
      let s = Bytes.create len in
      unsafe_blit_string (Obj.magic t.ptr) pos s 0 len;
      s
  let set_string t pos len v =
    let vlen = String.length v in
    let mlen = if len < vlen then len else vlen in
    unsafe_blit_string v 0 (Obj.magic t.ptr) pos mlen;
    unsafe_fill (Obj.magic t.ptr) (pos + mlen) (len - mlen) '\000'

  open Bigarray

  let get_int_0  t   = Int64.to_int (Obj.magic (t.ptr -  4) : int64)
  let get_int_1  t   = Int64.to_int (Obj.magic (t.ptr +  0) : int64)
  let get_int_2  t   = Int64.to_int (Obj.magic (t.ptr +  4) : int64)
  let get_int_3  t   = Int64.to_int (Obj.magic (t.ptr +  8) : int64)
  let get_int_4  t   = Int64.to_int (Obj.magic (t.ptr + 12) : int64)
  let get_int_5  t   = Int64.to_int (Obj.magic (t.ptr + 16) : int64)
  let get_int_6  t   = Int64.to_int (Obj.magic (t.ptr + 20) : int64)
  let get_int_7  t   = Int64.to_int (Obj.magic (t.ptr + 24) : int64)
  let get_int_8  t   = Int64.to_int (Obj.magic (t.ptr + 28) : int64)
  let get_int_9  t   = Int64.to_int (Obj.magic (t.ptr + 32) : int64)
  let get_int_10 t   = Int64.to_int (Obj.magic (t.ptr + 36) : int64)
  let get_int_11 t   = Int64.to_int (Obj.magic (t.ptr + 40) : int64)
  let get_int_12 t   = Int64.to_int (Obj.magic (t.ptr + 44) : int64)
  let get_int_13 t   = Int64.to_int (Obj.magic (t.ptr + 48) : int64)
  let get_int_14 t   = Int64.to_int (Obj.magic (t.ptr + 52) : int64)
  let get_int_15 t   = Int64.to_int (Obj.magic (t.ptr + 56) : int64)
  let set_int_0  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  0 (Int64.of_int v)
  let set_int_1  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  1 (Int64.of_int v)
  let set_int_2  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  2 (Int64.of_int v)
  let set_int_3  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  3 (Int64.of_int v)
  let set_int_4  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  4 (Int64.of_int v)
  let set_int_5  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  5 (Int64.of_int v)
  let set_int_6  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  6 (Int64.of_int v)
  let set_int_7  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  7 (Int64.of_int v)
  let set_int_8  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  8 (Int64.of_int v)
  let set_int_9  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  9 (Int64.of_int v)
  let set_int_10 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 10 (Int64.of_int v)
  let set_int_11 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 11 (Int64.of_int v)
  let set_int_12 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 12 (Int64.of_int v)
  let set_int_13 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 13 (Int64.of_int v)
  let set_int_14 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 14 (Int64.of_int v)
  let set_int_15 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 15 (Int64.of_int v)

  let get_int64_0  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 0
  let get_int64_1  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 1
  let get_int64_2  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 2
  let get_int64_3  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 3
  let get_int64_4  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 4
  let get_int64_5  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 5
  let get_int64_6  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 6
  let get_int64_7  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 7
  let get_int64_8  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 8
  let get_int64_9  t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 9
  let get_int64_10 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 10
  let get_int64_11 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 11
  let get_int64_12 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 12
  let get_int64_13 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 13
  let get_int64_14 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 14
  let get_int64_15 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 15
  let set_int64_0  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  0 v
  let set_int64_1  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  1 v
  let set_int64_2  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  2 v
  let set_int64_3  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  3 v
  let set_int64_4  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  4 v
  let set_int64_5  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  5 v
  let set_int64_6  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  6 v
  let set_int64_7  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  7 v
  let set_int64_8  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  8 v
  let set_int64_9  t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t)  9 v
  let set_int64_10 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 10 v
  let set_int64_11 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 11 v
  let set_int64_12 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 12 v
  let set_int64_13 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 13 v
  let set_int64_14 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 14 v
  let set_int64_15 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 15 v

  let get_float64_0  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  0
  let get_float64_1  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  1
  let get_float64_2  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  2
  let get_float64_3  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  3
  let get_float64_4  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  4
  let get_float64_5  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  5
  let get_float64_6  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  6
  let get_float64_7  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  7
  let get_float64_8  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  8
  let get_float64_9  t   = Array.unsafe_get (Obj.magic t.ptr : float array)  9
  let get_float64_10 t   = Array.unsafe_get (Obj.magic t.ptr : float array) 10
  let get_float64_11 t   = Array.unsafe_get (Obj.magic t.ptr : float array) 11
  let get_float64_12 t   = Array.unsafe_get (Obj.magic t.ptr : float array) 12
  let get_float64_13 t   = Array.unsafe_get (Obj.magic t.ptr : float array) 13
  let get_float64_14 t   = Array.unsafe_get (Obj.magic t.ptr : float array) 14
  let get_float64_15 t   = Array.unsafe_get (Obj.magic t.ptr : float array) 15
  let set_float64_0  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  0 v
  let set_float64_1  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  1 v
  let set_float64_2  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  2 v
  let set_float64_3  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  3 v
  let set_float64_4  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  4 v
  let set_float64_5  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  5 v
  let set_float64_6  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  6 v
  let set_float64_7  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  7 v
  let set_float64_8  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  8 v
  let set_float64_9  t v = Array.unsafe_set (Obj.magic t.ptr : float array)  9 v
  let set_float64_10 t v = Array.unsafe_set (Obj.magic t.ptr : float array) 10 v
  let set_float64_11 t v = Array.unsafe_set (Obj.magic t.ptr : float array) 11 v
  let set_float64_12 t v = Array.unsafe_set (Obj.magic t.ptr : float array) 12 v
  let set_float64_13 t v = Array.unsafe_set (Obj.magic t.ptr : float array) 13 v
  let set_float64_14 t v = Array.unsafe_set (Obj.magic t.ptr : float array) 14 v
  let set_float64_15 t v = Array.unsafe_set (Obj.magic t.ptr : float array) 15 v

  let get_int t i   = Int64.to_int (Obj.magic (t.ptr - (i - 1) * 4) : int64)
  let set_int t i v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) i (Int64.of_int v)
  let get_int64 t i   = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) i
  let set_int64 t i v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) i v
  let get_float64 t i   = Array.unsafe_get (Obj.magic t.ptr : float array) i
  let set_float64 t i v = Array.unsafe_set (Obj.magic t.ptr : float array) i v

  let unsafe_next t =
    let ptr = t.ptr + size64 in
    t.ptr <- ptr

  let next t =
    let ptr = t.ptr + size64 in
    if ptr > t.mem.Mem.data + t.mem.Mem.dim then
      raise (Invalid_argument "index out of bounds")
    else
      t.ptr <- ptr

  let unsafe_prev t =
    let ptr = t.ptr - size64 in
    t.ptr <- ptr

  let prev t =
    let ptr = t.ptr - size64 in
    if ptr < t.mem.Mem.data then
      raise (Invalid_argument "index out of bounds")
    else
      t.ptr <- ptr

  let unsafe_move t i = t.ptr <- t.mem.Mem.data + size64 * i

  let move t i =
    let ptr = t.mem.Mem.data + size64 * i in
    if ptr < t.mem.Mem.data || ptr > t.mem.Mem.data + t.mem.Mem.dim then
      raise (Invalid_argument "index out of bounds");
    t.ptr <- ptr

  let seek_float64 t pos ~min ~max v =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = t.mem.Mem.data + pos * 4 in
    let v' = ref 0. in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      v' := Array.unsafe_get (Obj.magic (data + !mid * size64) : float array) 0;
      if !v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Array.unsafe_get (Obj.magic (data + !min * size64) : float array) 0 in
    if v' >= v then !min else !max

  let seek_float64 t pos v =
    let data = t.mem.Mem.data + pos * 4 in
    let len = t.mem.Mem.dim / size64 in
    let size64 = size64 in
    let i = (t.ptr - data) / size64 in
    let v' = Array.unsafe_get (Obj.magic (data + pos * size64) : float array) 0 in
    let min = ref i in
    let max = ref i in
    let step = ref 1 in
    if v' < v then begin
      if !max < len - 1 then begin
        incr min;
        max := !min
      end;
      while !max < len
        && Array.unsafe_get (Obj.magic (data + !max * size64)) 0 < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      if !min > 0 then begin
        decr min;
        max := !min
      end;
      while !min > 0 && Array.unsafe_get (Obj.magic (data + !min * size64)) 0 > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_float64 t pos ~min:!min ~max:!max v else !max)

  let seek_int t pos ~min ~max v =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = t.mem.Mem.data + (pos - 1) * 4 in
    let v' = ref 0 in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      v' := Int64.to_int (Obj.magic (data + !mid * size64));
      if !v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Int64.to_int (Obj.magic (data + !min * size64)) in
    if v' >= v then !min else !max

  let seek_int t pos v =
    let data = t.mem.Mem.data in
    let len = t.mem.Mem.dim / size64 in
    let size64 = size64 in
    let i = (t.ptr - data) / size64 in
    let v' = Int64.to_int (Obj.magic (data + pos * size64)) in
    let min = ref i in
    let max = ref i in
    let step = ref 1 in
    if v' < v then begin
      if !max < len - 1 then begin
        incr min;
        max := !min
      end;
      while !max < len && Int64.to_int (Obj.magic (data + !max * size64)) < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      if !min > 0 then begin
        decr min;
        max := !min
      end;
      while !min > 0 && Int64.to_int (Obj.magic (data + !min * size64)) > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_int t pos ~min:!min ~max:!max v else !max)

  let seek_int64 t pos ~min ~max v =
    let mid = ref min in
    let min = ref min in
    let max = ref max in
    let data = t.mem.Mem.data + (pos - 1) * 4 in
    let v' = ref 0L in
    while !max > !min + 1 do
      mid := (!min + !max) asr 1;
      v' := Obj.magic (data + !mid * size64);
      if !v' < v then
        min := !mid
      else
        max := !mid
    done;
    let v' = Obj.magic (data + !min * size64) in
    if v' >= v then !min else !max

  let seek_int64 t pos v =
    let data = t.mem.Mem.data in
    let len = t.mem.Mem.dim / size64 in
    let size64 = size64 in
    let i = (t.ptr - data) / size64 in
    let v' = Obj.magic (data + pos * size64) in
    let min = ref i in
    let max = ref i in
    let step = ref 1 in
    if v' < v then begin
      if !max < len - 1 then begin
        incr min;
        max := !min
      end;
      while !max < len && Obj.magic (data + !max * size64) < v do
        max := !max + !step;
        step := !step * 2
      done;
      if !max >= len then max := len - 1
    end else if v' > v then begin
      if !min > 0 then begin
        decr min;
        max := !min
      end;
      while !min > 0 && Obj.magic (data + !min * size64) > v do
        min := !min - !step;
        step := !step * 2
      done;
      if !min < 0 then min := 0
    end;
    unsafe_move t (
      if !max > !min then seek_int64 t pos ~min:!min ~max:!max v else !max)

  module Array = struct
    type e = t
    type t = Mem.t

    let create len = (Obj.magic (Array1.create Char C_layout (len * size)) : Mem.t)

    let length t = t.Mem.dim / size64

    let unsafe_get t i =
      let data = t.Mem.data in
      { ptr = data + i * size64; mem = t; begin_ = data; end_ = data + t.Mem.dim }

    let init len f =
      let t = create len in
      let e = unsafe_get t 0 in
      for i = 0 to len - 2 do
        f i e;
        unsafe_next e
      done;
      f (len - 1) e;
      t

    let get t i =
      let ptr = t.Mem.data + i * size64 in
      let data = t.Mem.data in
      if i < 0 || ptr > data + t.Mem.dim then
        raise (Invalid_argument "index out of bounds");
      { ptr; mem = t; begin_ = data; end_ = data + t.Mem.dim }

    let unsafe_blit t t' = Array2.blit (Obj.magic t) (Obj.magic t')

    module H5tb = Hdf5_raw.H5tb

    let make_table t ?title ?chunk_size ?(compress = true) h5 dset_name =
      let title = match title with Some t -> t | None -> dset_name in
      let chunk_size = match chunk_size with Some s -> s | None -> length t in
      H5tb.make_table title (H5.hid h5) dset_name ~nrecords:(t.Mem.dim / size64)
        ~type_size:size ~field_names ~field_offset ~field_types ~chunk_size ~compress t

    let append_records t h5 dset_name =
      H5tb.append_records (H5.hid h5) dset_name ~nrecords:(t.Mem.dim / size64)
        ~type_size:size ~field_offset ~field_sizes t

    let read_table h5 table_name =
      let loc = H5.hid h5 in
      let nrecords = H5tb.get_table_info loc table_name in
      let t = create nrecords in
      H5tb.read_table loc table_name ~dst_size:size ~dst_offset:field_offset
        ~dst_sizes:field_sizes t;
      t

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

  let create () = Array.unsafe_get (Array.create 1) 0
  let mem t = t.mem

  module Vector = struct
    type e = t
    type t = {
      mutable mem : Mem.t;
      mutable capacity : int;
      mutable length : int;
      mutable end_ : e;
    }

    let create ?(capacity = 16) () =
      let mem = Array.create capacity in
      { mem; capacity; length = 0; end_ = Array.unsafe_get mem (-1) }

    let resize t capacity =
      if t.capacity > capacity then begin
        let mem = Array.create capacity in
        Array.unsafe_blit (Array1.sub (Obj.magic t.mem) 0 (capacity * size))
          (Obj.magic mem);
        t.mem <- mem
      end else if t.capacity < capacity then begin
        let mem = Array.create capacity in
        Array.unsafe_blit (Obj.magic t.mem)
          (Array1.sub (Obj.magic mem) 0 (t.capacity * size));
        t.mem <- mem
      end;
      t.capacity <- capacity

    let append t =
      if t.capacity = t.length then begin
        resize t (t.capacity * 2);
        t.end_ <- Array.unsafe_get t.mem (t.length - 1)
      end;
      t.length <- t.length + 1;
      unsafe_next t.end_;
      t.end_

    let unsafe_get (t : t) i = Array.unsafe_get t.mem i

    let iter (t : t) ~f =
      let ptr = t.end_ in
      unsafe_move ptr 0;
      for _ = 0 to t.length - 1 do
        f ptr;
        unsafe_next ptr
      done;
      unsafe_move ptr t.length

    let to_array t =
      let mem = Array.create t.length in
      Array.unsafe_blit (Array1.sub (Obj.magic t.mem) 0 (t.length * size)) mem;
      (Obj.magic mem : Mem.t)
  end
end

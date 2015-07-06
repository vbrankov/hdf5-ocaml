open Bigarray

module Type = struct
  type t =
  | Int
  | Int64
  | Float64
  | String of int
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

module Make(S : S) = struct
  include S

  let nfields = List.length S.fields
  let size64 = List.fold_left (fun size field ->
    match field.Field.type_ with
    | Type.Int | Type.Int64 | Type.Float64 -> size + 4
    | Type.String s -> size + (s + 7) / 8 * 4) 0 S.fields
  let size = 2 * size64

  module Mem = struct
    type t = {
      ops   : int;
      data  : int;
      flags : int;
      proxy : int;
      dim   : int;
    }
  end

  type t = {
    mutable ptr : int;
    mem         : Mem.t;
  }

  let unsafe_next t = t.ptr <- t.ptr + size64
  let unsafe_move t i = t.ptr <- t.mem.Mem.data + size64 * i

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

  let get_int_0 t   = Int64.to_int (Obj.magic (t.ptr -  4) : int64)
  let get_int_1 t   = Int64.to_int (Obj.magic (t.ptr +  0) : int64)
  let get_int_2 t   = Int64.to_int (Obj.magic (t.ptr +  4) : int64)
  let get_int_3 t   = Int64.to_int (Obj.magic (t.ptr +  8) : int64)
  let get_int_4 t   = Int64.to_int (Obj.magic (t.ptr + 12) : int64)
  let get_int_5 t   = Int64.to_int (Obj.magic (t.ptr + 16) : int64)
  let get_int_6 t   = Int64.to_int (Obj.magic (t.ptr + 20) : int64)
  let get_int_7 t   = Int64.to_int (Obj.magic (t.ptr + 24) : int64)
  let set_int_0 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 0 (Int64.of_int v)
  let set_int_1 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 1 (Int64.of_int v)
  let set_int_2 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 2 (Int64.of_int v)
  let set_int_3 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 3 (Int64.of_int v)
  let set_int_4 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 4 (Int64.of_int v)
  let set_int_5 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 5 (Int64.of_int v)
  let set_int_6 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 6 (Int64.of_int v)
  let set_int_7 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 7 (Int64.of_int v)

  let get_int64_0 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 0
  let get_int64_1 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 1
  let get_int64_2 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 2
  let get_int64_3 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 3
  let get_int64_4 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 4
  let get_int64_5 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 5
  let get_int64_6 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 6
  let get_int64_7 t = Array1.unsafe_get (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 7
  let set_int64_0 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 0 v
  let set_int64_1 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 1 v
  let set_int64_2 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 2 v
  let set_int64_3 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 3 v
  let set_int64_4 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 4 v
  let set_int64_5 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 5 v
  let set_int64_6 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 6 v
  let set_int64_7 t v = Array1.unsafe_set (Obj.magic (Obj.magic t - 4) : (int64, int64_elt, c_layout) Array1.t) 7 v

  let get_float64_0 t   = Array.unsafe_get (Obj.magic (t.ptr +  0) : float array) 0
  let get_float64_1 t   = Array.unsafe_get (Obj.magic (t.ptr +  4) : float array) 0
  let get_float64_2 t   = Array.unsafe_get (Obj.magic (t.ptr +  8) : float array) 0
  let get_float64_3 t   = Array.unsafe_get (Obj.magic (t.ptr + 12) : float array) 0
  let get_float64_4 t   = Array.unsafe_get (Obj.magic (t.ptr + 16) : float array) 0
  let get_float64_5 t   = Array.unsafe_get (Obj.magic (t.ptr + 20) : float array) 0
  let get_float64_6 t   = Array.unsafe_get (Obj.magic (t.ptr + 24) : float array) 0
  let get_float64_7 t   = Array.unsafe_get (Obj.magic (t.ptr + 28) : float array) 0
  let set_float64_0 t v = Array.unsafe_set (Obj.magic (t.ptr +  0) : float array) 0 v
  let set_float64_1 t v = Array.unsafe_set (Obj.magic (t.ptr +  4) : float array) 0 v
  let set_float64_2 t v = Array.unsafe_set (Obj.magic (t.ptr +  8) : float array) 0 v
  let set_float64_3 t v = Array.unsafe_set (Obj.magic (t.ptr + 12) : float array) 0 v
  let set_float64_4 t v = Array.unsafe_set (Obj.magic (t.ptr + 16) : float array) 0 v
  let set_float64_5 t v = Array.unsafe_set (Obj.magic (t.ptr + 20) : float array) 0 v
  let set_float64_6 t v = Array.unsafe_set (Obj.magic (t.ptr + 24) : float array) 0 v
  let set_float64_7 t v = Array.unsafe_set (Obj.magic (t.ptr + 28) : float array) 0 v

  module Array = struct
    type e = t
    type t = Mem.t

    let create len = (Obj.magic (Array1.create Char C_layout (len * size)) : Mem.t)

    let unsafe_get t i = { ptr = t.Mem.data + i * size64; mem = t }

    let unsafe_blit t t' = Array2.blit (Obj.magic t) (Obj.magic t')
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
        Array.unsafe_blit (Obj.magic t.mem) (Obj.magic mem);
        t.mem <- mem
      end;
      t.capacity <- capacity

    let append t =
      if t.capacity = t.length then
        resize t (t.capacity * 2);
      t.length <- t.length + 1;
      unsafe_next t.end_;
      t.end_

    let unsafe_get (t : t) i = Array.unsafe_get t.mem i

    let iter t ~f =
      let ptr = t.end_ in
      unsafe_move ptr 0;
      for _ = 0 to t.length - 1 do
        f ptr;
        unsafe_next ptr
      done;
      unsafe_move ptr t.length

    let compact t =
      let mem = Array.create t.length in
      Array.unsafe_blit (Array1.sub (Obj.magic t.mem) 0 (t.length * size)) mem;
      (Obj.magic mem : Mem.t)
  end
end

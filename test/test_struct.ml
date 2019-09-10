open Hdf5_caml

module Record = struct
  [%%h5struct
    sf64 "sf64" Float64     Seek;
    si   "si"   Int         Seek;
    si64 "si64" Int64       Seek;
    ss   "ss"   (String 14) Seek;
    f64  "f64"  Float64;
    i    "i"    Int;
    i64  "i64"  Int64;
    s    "s"    (String 16)]
end

module Simple = struct
  [%%h5struct f "F" Float64 Seek]
end

let () =
  let len = 1000 in
  let a = Record.Array.init len (fun i e ->
    let si = i * 2 in
    Record.set e
      ~sf64:(float_of_int si) ~si ~si64:(Int64.of_int si) ~ss:(Printf.sprintf "%14d" si)
      ~f64:(float_of_int i) ~i ~i64:(Int64.of_int i) ~s:(Printf.sprintf "%14d" i)) in
  let assert_val e i =
    let si = i * 2 in
    assert(Record.sf64 e = float_of_int si);
    assert(Record.si   e = si);
    assert(Record.si64 e = Int64.of_int si);
    assert(Record.ss   e = Printf.sprintf "%14d" si);
    assert(Record.f64  e = float_of_int i);
    assert(Record.i    e = i);
    assert(Record.i64  e = Int64.of_int i);
    assert(Record.s    e = Printf.sprintf "%14d" i);
    assert(Record.pos  e = i)
  in
  for i = 0 to len - 1 do
    let e = Record.Array.get a i in
    assert_val e i
  done;
  let e = Record.Array.get a 0 in
  for i = 0 to len - 2 do
    assert_val e i;
    Record.next e
  done;
  assert_val e (len - 1);
  for i = len - 1 downto 1 do
    assert_val e i;
    Record.prev e
  done;
  assert_val e 0;
  for i = 0 to len - 1 do
    Record.move e i;
    assert_val e i
  done;

  let r = Array.init len (fun i -> i) in
  for i = 0 to len - 2 do
    let j = i + Random.int (len - i) in
    let r_i = r.(i) in
    r.(i) <- r.(j);
    r.(j) <- r_i
  done;

  for i = 0 to len - 1 do
    Record.seek_sf64 e (float_of_int (r.(i) * 2));
    assert_val e r.(i);
    Record.seek_sf64 e (float_of_int (r.(i) * 2 + 1));
    assert_val e r.(i)
  done;
  for i = 0 to len - 1 do
    Record.seek_si e (r.(i) * 2);
    assert_val e r.(i);
    Record.seek_si e (r.(i) * 2 + 1);
    assert_val e r.(i)
  done;
  for i = 0 to len - 1 do
    Record.seek_si64 e (Int64.of_int (r.(i) * 2));
    assert_val e r.(i);
    Record.seek_si64 e (Int64.of_int (r.(i) * 2 + 1));
    assert_val e r.(i)
  done;
  for i = 0 to len - 1 do
    Record.seek_ss e (Printf.sprintf "%14d" (r.(i) * 2));
    assert_val e r.(i);
    Record.seek_ss e (Printf.sprintf "%14d" (r.(i) * 2 + 1));
    assert_val e r.(i)
  done;
  for _ = 0 to len - 1 do
    let f = Random.float (float_of_int len) in
    Record.seek_sf64 e f;
    assert (Record.sf64 e <= f);
    assert (Record.sf64 e +. 2. > f);
    Record.seek_sf64 e f;
    assert (Record.sf64 e <= f);
    assert (Record.sf64 e +. 2. > f);
  done;
  for _ = 0 to len - 1 do
    let i = Random.int (len * 2) in
    Record.seek_si e i;
    assert (Record.si e <= i);
    assert (Record.si e + 2 > i);
    Record.seek_si e i;
    assert (Record.si e <= i);
    assert (Record.si e + 2 > i);
  done;
  for _ = 0 to len - 1 do
    let i = Int64.of_int (Random.int len) in
    Record.seek_si64 e i;
    assert (Record.si64 e <= i);
    assert (Int64.add (Record.si64 e) 2L > i);
    Record.seek_si64 e i;
    assert (Record.si64 e <= i);
    assert (Int64.add (Record.si64 e) 2L > i);
  done;

  let v = Record.Vector.create () in
  begin
    try
      let _ = Record.Vector.get v 0 in
      assert false
    with Invalid_argument _ -> ()
  end;
  let _ = Record.Vector.append v in
  let e = Record.Vector.get v 0 in
  for i = 0 to len - 1 do
    let e' = Record.Vector.append v in
    Record.next e;
    Record.set_i e' i;
    assert (Record.i e = i);
    assert (Record.Vector.end_ v |> Record.i = i)
  done;

  let h5 = H5.create_trunc "test.h5" in
  Record.Array.make_table a h5 "f\\o/o";
  Record.Array.write a h5 "b\\a/r";
  H5.close h5;

  let h5 = H5.open_rdonly "test.h5" in
  assert (H5.ls ~order:INC h5 = ["b\\a/r"; "f\\o/o"]);
  Record.Array.read_table h5 "f\\o/o"
  |> Record.Array.iteri ~f:(fun i e ->
    assert_val e i);
  Record.Array.read h5 "b\\a/r"
  |> Record.Array.iteri ~f:(fun i e ->
    assert_val e i);
  H5.close h5;

  let slen = 1024 * 1024 in
  let s = Simple.Array.make slen in
  let h5 = H5.create_trunc "test.h5" in
  let a = Record.Array.make 0 in
  Record.Array.make_table a h5 "a";
  Record.Array.write a h5 "b";
  Simple.Array.make_table s h5 "c";
  Simple.Array.write s h5 "d";
  H5.close h5;

  let h5 = H5.open_rdonly "test.h5" in
  let a = Record.Array.read_table h5 "a" in
  assert (Record.Array.length a = 0);
  let a = Record.Array.read h5 "b" in
  assert (Record.Array.length a = 0);
  let s = Simple.Array.read_table h5 "c" in
  assert (Simple.Array.length s = slen);
  let s = Simple.Array.read h5 "d" in
  assert (Simple.Array.length s = slen);
  H5.close h5;

  (* This test used to trigger a segfault when [Ext.t] leaked in [Struct.set_string] and
     [Struct.Vector.realloc]. *)
  for _ = 0 to 7 do
    let v = Record.Vector.create () in
    let s = "" in
    for i = 0 to 16 * 1024 * 1024 - 1 do
      let f = float i in
      let i64 = Int64.of_int i in
      Record.set (Record.Vector.append v) ~sf64:f ~si:i ~si64:i64 ~ss:s ~f64:f ~i ~i64 ~s;
      let _ = Record.Vector.get v (Record.Vector.length v - 1) in ()
    done;
    Gc.full_major ()
  done;

  (* This test used to trigger a segmentation fault when [Vector.capacity] showed bigger
     size than was actually alloced in [Vector.mem]. *)
  let v = Record.Vector.create () in
  let _ = Record.Vector.append v in
  Struct.reset_serialize ();
  let s = Marshal.to_string v [Closures] in
  Gc.full_major ();
  Struct.reset_deserialize ();
  let v = Marshal.from_string s 0 in
  Record.Vector.append v
  |> Record.set ~sf64:0. ~si:0 ~si64:0L ~ss:"              " ~f64:0. ~i:0 ~i64:0L
    ~s:"                ";

  let _ = Marshal.to_string (module Record : Hdf5_caml.Struct_intf.S) [Closures] in

  let v = Simple.Vector.create () in
  for _ = 0 to 15 do
    Simple.Vector.append v
    |> Simple.set ~f:0.
  done;
  Simple.Vector.clear v;
  let e = Simple.Vector.append v in
  Simple.seek_f e 3.;
  assert (Simple.pos e = 0)

module Big = struct
  [%%h5struct
    id   "ID"   Int;
    big  "Big"  Bigstring;
    f32  "F32"  Array_float32;
    f64  "F64"  Array_float64;
    si8  "SI8"  Array_sint8;
    ui8  "UI8"  Array_uint8;
    si16 "SI16" Array_sint16;
    ui16 "UI16" Array_uint16;
    i32  "I32"  Array_int32;
    i64  "I64"  Array_int64;
    i    "I"    Array_int;
    ni   "NI"   Array_nativeint;
    c    "C"    Array_char;
  ]
end

open Bigarray

let create_array len =
  Big.Array.init len (fun i b ->
    let f32 = Array1.create float32 c_layout i in
    for j = 0 to i - 1 do
      f32.{j} <- float j
    done;
    let f64 = Array1.create float64 c_layout i in
    for j = 0 to i - 1 do
      f64.{j} <- float j
    done;
    let si8 = Array1.create int8_signed c_layout i in
    for j = 0 to i - 1 do
      si8.{j} <- j
    done;
    let ui8 = Array1.create int8_unsigned c_layout i in
    for j = 0 to i - 1 do
      ui8.{j} <- j
    done;
    let si16 = Array1.create int16_signed c_layout i in
    for j = 0 to i - 1 do
      si16.{j} <- j
    done;
    let ui16 = Array1.create int16_unsigned c_layout i in
    for j = 0 to i - 1 do
      ui16.{j} <- j
    done;
    let i32 = Array1.create int32 c_layout i in
    for j = 0 to i - 1 do
      i32.{j} <- Int32.of_int j
    done;
    let i64 = Array1.create int64 c_layout i in
    for j = 0 to i - 1 do
      i64.{j} <- Int64.of_int j
    done;
    let ai = Array1.create int c_layout i in
    for j = 0 to i - 1 do
      ai.{j} <- j
    done;
    let ni = Array1.create nativeint c_layout i in
    for j = 0 to i - 1 do
      ni.{j} <- Nativeint.of_int j
    done;
    let c = Array1.create char c_layout i in
    for j = 0 to i - 1 do
      c.{j} <- Char.chr (j land 0xff)
    done;
    Big.set b ~id:i ~big:(string_of_int i |> Struct.Bigstring.of_string) ~f32 ~f64 ~si8
      ~ui8 ~si16 ~ui16 ~i32 ~i64 ~i:ai ~ni ~c)

let stress_test_bigarray num_arrays num_elements =
  let create_array () =
    Big.Array.get (create_array (1 + Random.int num_arrays)) 0 in
  let a = Array.init num_arrays (fun _ -> create_array ()) in
  let create_element () =
    let a = a.(Random.int num_arrays) in
    let pos = Big.mem a |> Big.Array.length |> Random.int in
    Big.move a pos;
    let big = Big.big a in
    assert (Struct.Bigstring.to_string big = string_of_int pos);
    let f32 = Big.f32 a in
    assert (Array1.dim f32 = pos);
    for i = 0 to pos - 1 do
      assert (f32.{i} = float_of_int i)
    done;
    let si16 = Big.si16 a in
    assert (Array1.dim si16 = pos);
    for i = 0 to pos - 1 do
      assert (si16.{i} = i)
    done;
    big in
  let e = Array.init num_elements (fun _ -> create_element ()) in
  Struct.reset_serialize ();
  let marshalled = ref (Marshal.to_string a.(Random.int num_arrays) []) in
  let create_simple () =
    let h5 = H5.create_trunc "simple.h5" in
    Big.Array.write (Big.mem a.(Random.int num_arrays)) h5 "a";
    H5.close h5 in
  create_simple ();
  let create_table () =
    let h5 = H5.create_trunc "table.h5" in
    Big.Array.make_table (Big.mem a.(Random.int num_arrays)) h5 "a";
    H5.close h5 in
  create_table ();
  for _ = 0 to num_arrays - 1 do
    for _ = 0 to num_elements - 1 do
      match Random.int 32 with
      | 0 -> a.(Random.int num_arrays) <- create_array ()
      | 1 ->
        Struct.reset_serialize ();
        marshalled := Marshal.to_string a.(Random.int num_arrays) []
      | 2 ->
        Struct.reset_deserialize ();
        a.(Random.int num_arrays) <- Marshal.from_string !marshalled 0
      | 3 -> create_simple ()
      | 4 ->
        let h5 = H5.open_rdonly "simple.h5" in
        a.(Random.int num_arrays) <- Big.Array.(get (read h5 "a") 0);
        H5.close h5
      | 5 -> create_table ()
      | 6 ->
        let h5 = H5.open_rdonly "table.h5" in
        a.(Random.int num_arrays) <- Big.Array.(get (read_table h5 "a") 0);
        H5.close h5
      | _ -> e.(Random.int num_elements) <- create_element ()
    done;
    Gc.full_major ()
  done

let () =
  stress_test_bigarray 128 128

let () =
  let s =
    Array.init 128 (fun i ->
      String.init i (fun i -> Char.chr (i + 1))) in
  let b = Array.map Struct.Bigstring.of_string s in
  Gc.full_major ();
  Array.iteri (fun i b ->
    assert (Struct.Bigstring.to_string b = s.(i))) b

module Bigchar = struct
  [%%h5struct
    id "ID" Int;
    bc "BC" Array_char;
  ]
end

let () =
  let s =
    Array.init 1024 (fun i ->
      String.init i (fun i -> Char.chr (i land 0xff))) in
  let v = Bigchar.Vector.create () in
  for i = 0 to 1023 do
    Bigchar.Vector.append v
    |> Bigchar.set ~id:i ~bc:(Struct.Array_char.of_string s.(i))
  done;
  let a = Bigchar.Vector.to_array v in
  Gc.full_major ();
  Bigchar.Array.iteri a ~f:(fun i a ->
    assert (Bigchar.bc a |> Struct.Array_char.to_string = s.(i)))

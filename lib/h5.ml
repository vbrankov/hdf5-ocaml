module Time = struct
  type t = int64
end

module Iter_order = struct
  type t =
  | INC
  | DEC
  | NATIVE
  | N
end

module Iter = struct
  type t =
  | CONT
  | STOP
end

module Index = struct
  type t =
  | NAME
  | CRT_ORDER
  | N
end

module Ih_info = struct
  type t = {
    index_size : int;
    heap_size  : int }
end

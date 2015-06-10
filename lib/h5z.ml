module Filter = struct
  type t =
  | NONE
  | DEFLATE
  | SHUFFLE
  | FLETCHER32
  | SZIP
  | NBIT
  | SCALEOFFSET
  | CUSTOM of int
end

module Flag = struct
  type t =
  | MANDATORY
  | OPTIONAL
  | REVERSE
  | SKIP_EDC
end

type t = { tp : string; sz : int; indent : string; end_pos : int }

let is_recursive = function
  | "clip" -> true
  | "dinf" -> true
  | "edts" -> true
  | "imap" -> true
  | "  in" -> true
  | "matt" -> true
  | "mdia" -> true
  | "minf" -> true
  | "moov" -> true
  | "rmda" -> true
  | "rmra" -> true
  | "stbl" -> true
  | "trak" -> true
  | "tref" -> true
  | _ -> false


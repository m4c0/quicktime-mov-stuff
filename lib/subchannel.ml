type t = int * in_channel

let from_channel (ic : in_channel) : t = (in_channel_length ic), ic

let open_in (file : string) : t = file |> open_in_bin |> from_channel

let close_in ((_, ic) : t) : unit = Stdlib.close_in ic
let close_in_noerr ((_, ic) : t) : unit = Stdlib.close_in_noerr ic

let is_empty ((lim, ic) : t) : bool = (pos_in ic) == lim

let pos_in ((_, ic) : t) : int = Stdlib.pos_in ic

let input_binary_int ((_, ic) : t) : int = Stdlib.input_binary_int ic
let input_fourcc ((_, ic) : t) : string = Stdlib.really_input_string ic 4

let seek_in_end ((l, ic) : t) : unit = Stdlib.seek_in ic l

let limit_of ((l, _) : t) : int = l

let limit_by ((_, ic) : t) (l : int) : t =
  let ll = l + (Stdlib.pos_in ic) in
  (ll, ic)


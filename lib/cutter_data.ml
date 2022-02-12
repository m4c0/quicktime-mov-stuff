module type Scale = sig
  type t = private int
  val of_int : int -> t
  val to_int : t -> int
end
module MediaScale : Scale = struct
  type t = int
  let of_int x = x
  let to_int x = x
end
module MovieScale : Scale = struct
  type t = int
  let of_int x = x
  let to_int x = x
end

type md_duration = { value: int; scale: MediaScale.t }
type mv_duration = { value: int; scale: MovieScale.t }

type edit = { dur: mv_duration; mtime: md_duration; mrate: float }
type track = { tkhd: mv_duration; mdhd: md_duration; edts: edit list list }
type movie = { mvhd: mv_duration; traks: track list }

let time_of_vs value scale : int * int * int * int =
  let ms = 100000 * (value mod scale) / scale in
  let secs = value / scale in
  let sec = secs mod 60 in
  let mins = secs / 60 in
  let min = mins mod 60 in
  let hour = mins / 60 in
  (hour, min, sec, ms)

let time_of_mdd ({ value; scale } : md_duration) = time_of_vs value (MediaScale.to_int scale)
let time_of_mvd ({ value; scale } : mv_duration) = time_of_vs value (MovieScale.to_int scale)

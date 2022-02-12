module type Scale = sig
  type t = private int
  val of_int : int -> t
  val to_int : t -> int
end
module BaseScale : Scale = struct
  type t = int
  let of_int x = x
  let to_int x = x
end

module Duration (S : Scale) = struct
  type t = { value: int; scale: S.t }

  let with_value (v : int) (tt : t) = { tt with value = v }
  let maps (fn : int -> int -> int) (tt : t) =
    let s = S.to_int tt.scale in
    { tt with value = fn tt.value s }
end

module MediaScale : Scale = struct
  include BaseScale
end
module MediaDuration = Duration(MediaScale)

module MovieScale : Scale = struct
  include BaseScale
end
module MovieDuration = Duration(MovieScale)

type md_duration = MediaDuration.t
type mv_duration = MovieDuration.t

type edit = { dur: mv_duration; mtime: md_duration; mrate: float }
type track = { tkhd: mv_duration; mdhd: md_duration; edts: edit list }
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

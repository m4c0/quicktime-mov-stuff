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

  let scale_of ({ scale; _ } : t) = S.to_int scale

  let maps (fn : int -> int -> int) (tt : t) =
    let s = S.to_int tt.scale in
    { tt with value = fn tt.value s }

  let fold_left fn (l : t list) : t =
    let hd = List.hd l in
    let tl = List.tl l in
    List.fold_left fn hd tl

  let max_of (l : t list) : t =
    let max (a : t) (b : t) = 
      if a.value / scale_of a > b.value / scale_of b
      then a
      else b
    in
    fold_left max l

  let sum_all (l : t list) : t =
    let sum (a : t) (b : t) = {
      value = (a.value * (scale_of b) / (scale_of a)) + b.value;
      scale = b.scale;
    } in
    fold_left sum l

  let with_value (v : int) (tt : t) = { tt with value = v }
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

type track_head = { dur: mv_duration; vol: int }

type edit = { dur: mv_duration; mtime: md_duration; mrate: float }
type track = { tkhd: track_head; mdhd: md_duration; edts: edit list }
type movie = { mvhd: mv_duration; traks: track list }

let duration_of_edts (edts : edit list) : mv_duration =
  List.map (fun e -> e.dur) edts 
  |> MovieDuration.sum_all

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

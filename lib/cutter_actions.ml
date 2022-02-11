type header = { dur: int; scale: int }
type track = { dur: int }
type movie = {
  mvhd : header;
  traks : track list
}

let tree : Atoms.t list ref = ref []

let i32 bs n = Bytes.get_int32_be bs n |> Int32.to_int

let debug_dur dur ts =
  let ms = 100000 * (dur mod ts) / ts in
  let sec = dur / ts in
  let min = sec / 60 in
  let hour = min / 60 in
  Printf.printf "    Duration: %02d:%02d:%02d.%06d\n" hour (min mod 60) (sec mod 60) ms

let debug_track (m : movie) (t : track) =
  print_endline "Track:";
  debug_dur t.dur m.mvhd.scale

let debug_movie (m : movie) =
  print_endline "Movie:";
  debug_dur m.mvhd.dur m.mvhd.scale;
  List.iter (debug_track m) m.traks

let header_of_mvhd mvhd : header = {
  scale = i32 mvhd 12;
  dur = i32 mvhd 16;
}

let duration_of_tkhd tkhd = i32 tkhd 20

let track_of_trak trak : track =
  let dur = Atoms.find_leaf_atom "tkhd" trak |> duration_of_tkhd in
  { dur }

let movie_from_moov moov : movie =
  let mvhd = Atoms.find_leaf_atom "mvhd" moov |> header_of_mvhd in
  let traks = Atoms.find_all_node_atoms "trak" moov |> List.map track_of_trak in
  { mvhd; traks }

let load file =
  tree := Atoms.from_file file;
  let m = Atoms.find_node_atom "moov" !tree |> movie_from_moov in
  debug_movie m


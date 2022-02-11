open Cutter_data

let dur dur ts =
  let ms = 100000 * (dur mod ts) / ts in
  let sec = dur / ts in
  let min = sec / 60 in
  let hour = min / 60 in
  Printf.printf "    Duration: %02d:%02d:%02d.%06d\n" hour (min mod 60) (sec mod 60) ms

let track (m : movie) (t : track) =
  print_endline "Track:";
  dur t.dur m.mvhd.scale

let movie (m : movie) =
  print_endline "Movie:";
  dur m.mvhd.dur m.mvhd.scale;
  List.iter (track m) m.traks


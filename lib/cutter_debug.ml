open Cutter_data

let dur title dur ts =
  let ms = 100000 * (dur mod ts) / ts in
  let sec = dur / ts in
  let min = sec / 60 in
  let hour = min / 60 in
  Printf.printf "    %s: %02d:%02d:%02d.%06d\n" title hour (min mod 60) (sec mod 60) ms

let edit (e : edit) =
  Printf.printf "%d %d %f\n" e.dur e.mtime e.mrate

let track (m : movie) (t : track) =
  print_endline "Track:";
  dur "Duration" t.dur m.mvhd.scale;
  dur "Media duration" t.media.dur t.media.scale;
  List.iter (List.iter edit) t.edits

let movie (m : movie) =
  print_endline "Movie:";
  dur "Duration" m.mvhd.dur m.mvhd.scale;
  List.iter (track m) m.traks


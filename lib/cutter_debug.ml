open Cutter_data

let dur title hour min sec ms =
  Printf.printf "    %s: %02d:%02d:%02d.%06d\n" title hour min sec ms

let dur_md (title : string) (d : md_duration)  =
  let (hour, min, sec, ms) = time_of_mdd d in
  dur title hour min sec ms
let dur_mv (title : string) (d : mv_duration)  =
  let (hour, min, sec, ms) = time_of_mvd d in
  dur title hour min sec ms

let edit (start : mv_duration) (e : edit) =
  dur_mv "- Start at" start;
  dur_mv "  Track duration" e.dur;
  dur_md "  Media time" e.mtime;
  Printf.printf "      Media rate: %f\n" e.mrate;
  MovieDuration.sum_all [start; e.dur]

let track (t : track) =
  print_endline "Track:";
  dur_mv "Duration" t.tkhd.dur;
  dur_md "Media duration" t.mdhd;
  Printf.printf "    Volume: %d%%\n" (t.tkhd.vol * 100 / 256);
  print_endline "    Edits:";
  List.fold_left edit (MovieDuration.with_value 0 t.tkhd.dur) t.edts
  |> ignore

let movie (m : movie) =
  print_endline "Movie:";
  dur_mv "Duration" m.mvhd.dur;
  Printf.printf "Volume: %d%%\n" (m.mvhd.vol * 100 / 256);
  List.iter track m.traks


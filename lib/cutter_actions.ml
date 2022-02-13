open Cutter_data

let tree : Atoms.t list ref = ref []

let load file =
  tree := Atoms.from_file file;
  Cutter_parser.tree !tree |> Cutter_debug.movie

let ltrim trk edt secs =
  let chg_edit i e : edit =
    if i = edt
    then 
      let dur = MovieDuration.maps (fun d s -> d - secs * s) e.dur in
      let mtime = MediaDuration.maps (fun d s -> d + secs * s) e.mtime in
      { e with dur; mtime }
    else e
  in
  let chg_trak i t : track =
    if i = trk
    then 
      let edts = List.mapi chg_edit t.edts in
      let tkhd = duration_of_edts edts in
      { t with tkhd; edts }
    else t
  in
  let chg_movie m =
    let traks = List.mapi chg_trak m.traks in
    let dur_of (t : track) = t.tkhd in
    let mvhd = List.map dur_of traks |> MovieDuration.max_of in
    { mvhd; traks }
  in
  let t = !tree in
  tree := Cutter_parser.tree t |> chg_movie |> Cutter_writer.tree t;
  Cutter_parser.tree !tree |> Cutter_debug.movie

let play () =
  Atoms.to_file "/tmp/m4c0.cutter.test.mov" !tree;
  Unix.system "open /tmp/m4c0.cutter.test.mov" |> ignore;
  print_endline "done"

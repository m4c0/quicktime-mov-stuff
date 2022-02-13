open Cutter_data

let tree : Atoms.t list ref = ref []

let load file =
  tree := Atoms.from_file file;
  Cutter_parser.tree !tree |> Cutter_debug.movie

let apply_edit trk edt efn =
  let mapi idx fn l =
    let mapi_fn idx fn i e = if i == idx then fn e else e in
    List.mapi (mapi_fn idx fn) l
  in
  let lt_trak t : track =
    let edts = mapi edt efn t.edts in
    let tkhd = duration_of_edts edts in
    { t with tkhd; edts }
  in
  let chg_movie m =
    let traks = mapi trk lt_trak m.traks in
    let dur_of (t : track) = t.tkhd in
    let mvhd = List.map dur_of traks |> MovieDuration.max_of in
    { mvhd; traks }
  in
  let t = !tree in
  tree := Cutter_parser.tree t |> chg_movie |> Cutter_writer.tree t;
  Cutter_parser.tree !tree |> Cutter_debug.movie

let ltrim trk edt secs =
  let lt_edit e : edit = 
    let dur = MovieDuration.maps (fun d s -> d - secs * s) e.dur in
    let mtime = MediaDuration.maps (fun d s -> d + secs * s) e.mtime in
    { e with dur; mtime }
  in
  apply_edit trk edt lt_edit

let rtrim trk edt secs =
  let lt_edit e : edit = 
    let dur = MovieDuration.maps (fun d s -> d - secs * s) e.dur in
    { e with dur }
  in
  apply_edit trk edt lt_edit

let play () =
  Atoms.to_file "/tmp/m4c0.cutter.test.mov" !tree;
  Unix.system "open /tmp/m4c0.cutter.test.mov" |> ignore;
  print_endline "done"

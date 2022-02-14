open Cutter_data

let tree : Atoms.t list ref = ref []

let dump () =
  Cutter_parser.tree !tree |> Cutter_debug.movie

let load file =
  tree := Atoms.from_file file;
  Cutter_undo.purge ();
  dump ()

let apply_edit trk edt efn =
  let mapi idx fn l =
    let mapi_fn idx fn i e = if i == idx then fn e else [ e ] in
    let res = List.mapi (mapi_fn idx fn) l |> List.concat in
    if List.length res > 0
    then res
    else failwith "can't leave a track with no edits"
  in
  let lt_trak t : track list =
    let edts = mapi edt efn t.edts in
    let tkhd = duration_of_edts edts in
    [ { t with tkhd; edts } ]
  in
  let chg_movie m =
    let traks = mapi trk lt_trak m.traks in
    let dur_of (t : track) = t.tkhd in
    let mvhd = List.map dur_of traks |> MovieDuration.max_of in
    { mvhd; traks }
  in
  let t = !tree in
  tree := Cutter_parser.tree t |> Cutter_undo.add_undo |> chg_movie |> Cutter_writer.tree t;
  dump ()

let ltrim trk edt secs =
  let lt_edit e : edit list = 
    let dur = MovieDuration.maps (fun d s -> d - secs * s) e.dur in
    let mtime = MediaDuration.maps (fun d s -> d + secs * s) e.mtime in
    [ { e with dur; mtime } ]
  in
  apply_edit trk edt lt_edit

let rtrim trk edt secs =
  let lt_edit e : edit list = 
    let dur = MovieDuration.maps (fun d s -> d - secs * s) e.dur in
    [ { e with dur } ]
  in
  apply_edit trk edt lt_edit

let delete trk edt = apply_edit trk edt (fun _ -> [])

let split trk edt secs =
  let first e : edit =
    let dur = MovieDuration.maps (fun _ s -> secs * s) e.dur in
    { e with dur }
  in
  let second e : edit =
    let dur = MovieDuration.maps (fun d s -> d - secs * s) e.dur in
    let mtime = MediaDuration.maps (fun d s -> d + secs * s) e.mtime in
    { e with dur; mtime }
  in
  apply_edit trk edt (fun e -> [ first e; second e ])

let play () =
  Atoms.to_file "/tmp/m4c0.cutter.test.mov" !tree;
  Unix.system "open /tmp/m4c0.cutter.test.mov" |> ignore;
  print_endline "done"

let undo () =
  tree := Cutter_parser.tree !tree |> Cutter_undo.undo |> Cutter_writer.tree !tree;
  dump ()

let redo () = 
  tree := Cutter_parser.tree !tree |> Cutter_undo.redo |> Cutter_writer.tree !tree;
  dump ()

let foreach_track fn =
  let t = Cutter_parser.tree !tree |> Cutter_undo.add_undo in
  List.iteri (fun i _ -> fn i; Cutter_undo.pop_undo ()) t.traks

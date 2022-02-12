open Cutter_data

let tree : Atoms.t list ref = ref []

let map_tree fn =
  let m = Atoms.find_node_atom "moov" !tree |> Cutter_parser.moov |> fn in
  Cutter_debug.movie m

let add_md_dur (dur : md_duration) secs = 
  let fn d s = d + (secs * s) in
  MediaDuration.maps fn dur
let add_mv_dur (dur : mv_duration) secs =
  let fn d s = d + (secs * s) in
  MovieDuration.maps fn dur

let load file =
  tree := Atoms.from_file file;
  map_tree (fun x -> x)

let ltrim trk edt secs =
  let chg_edit i e : edit =
    if i = edt
    then { e with dur = add_mv_dur e.dur (-secs); mtime = add_md_dur e.mtime secs }
    else e
  in
  let chg_trak i t : track =
    if i = trk
    then 
      let edts = List.mapi chg_edit t.edts in
      let dur_of (e : edit) = e.dur in
      let tkhd = List.map dur_of edts |> MovieDuration.sum_all in
      { t with tkhd; edts }
    else t
  in
  let chg_movie m =
    let traks = List.mapi chg_trak m.traks in
    let dur_of (t : track) = t.tkhd in
    let mvhd = List.map dur_of traks |> MovieDuration.max_of in
    { mvhd; traks }
  in
  map_tree chg_movie

let play () =
  Atoms.to_file "/tmp/m4c0.cutter.test.mov" !tree;
  Unix.system "open /tmp/m4c0.cutter.test.mov" |> ignore;
  print_endline "done"

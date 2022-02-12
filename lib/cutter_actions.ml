open Cutter_data

let tree : Atoms.t list ref = ref []

let i32 bs n = Bytes.get_int32_be bs n |> Int32.to_int

let header_of_mdhd mvhd : md_duration = 
  let scale = i32 mvhd 12 |> MediaScale.of_int in
  let value = i32 mvhd 16 in
  { scale; value }

let header_of_mvhd mvhd : mv_duration = 
  let scale = i32 mvhd 12 |> MovieScale.of_int in
  let value = i32 mvhd 16 in
  { scale; value }

let edit_of_elst (mvhd : mv_duration) (mdhd : md_duration) elst : edit list =
  let entry x : edit =
    let p = 8 + x * 12 in
    let dur = i32 elst p in
    let mtime = i32 elst (p + 4) in
    let mrate = (float_of_int (i32 elst (p + 8))) /. 65536.0 in
    {
      dur = { value = dur; scale = mvhd.scale };
      mtime = { value = mtime; scale = mdhd.scale };
      mrate 
    }
  in
  let rec entries = function
    | 0 -> []
    | x -> entry (x - 1) :: entries (x - 1)
  in
  let qty = i32 elst 4 in
  entries qty |> List.rev

let duration_of_tkhd mvhd tkhd : mv_duration = {
  value = i32 tkhd 20;
  scale = mvhd.scale
}

let track_of_trak mvhd trak : track =
  let tkhd = Atoms.find_leaf_atom "tkhd" trak |> duration_of_tkhd mvhd in
  let mdhd = Atoms.find_node_atom "mdia" trak |> Atoms.find_leaf_atom "mdhd" |> header_of_mdhd in
  let edts = Atoms.find_node_atom "edts" trak |> Atoms.find_leaf_atom "elst" |> edit_of_elst mvhd mdhd in
  { tkhd; mdhd; edts }

let movie_from_moov moov : movie =
  let mvhd = Atoms.find_leaf_atom "mvhd" moov |> header_of_mvhd in
  let traks = Atoms.find_all_node_atoms "trak" moov |> List.map (track_of_trak mvhd) in
  { mvhd; traks }

let load file =
  tree := Atoms.from_file file;
  let m = Atoms.find_node_atom "moov" !tree |> movie_from_moov in
  Cutter_debug.movie m

let play () =
  Atoms.to_file "/tmp/m4c0.cutter.test.mov" !tree;
  Unix.system "open /tmp/m4c0.cutter.test.mov" |> ignore;
  print_endline "done"

open Cutter_data

let tree : Atoms.t list ref = ref []

let i32 bs n = Bytes.get_int32_be bs n |> Int32.to_int

let header_of_mvhd mvhd : header = {
  scale = i32 mvhd 12;
  dur = i32 mvhd 16;
}
let edit_of_elst elst : edit list =
  let entry x : edit =
    let p = 8 + x * 12 in
    let dur = i32 elst p in
    let mtime = i32 elst (p + 4) in
    let mrate = (float_of_int (i32 elst (p + 8))) /. 65536.0 in
    { dur; mtime; mrate }
  in
  let rec entries = function
    | 0 -> []
    | x -> entry (x - 1) :: entries (x - 1)
  in
  let qty = i32 elst 4 in
  entries qty |> List.rev

let duration_of_tkhd tkhd = i32 tkhd 20

let track_of_trak trak : track =
  let dur = Atoms.find_leaf_atom "tkhd" trak |> duration_of_tkhd in
  let media = Atoms.find_node_atom "mdia" trak |> Atoms.find_leaf_atom "mdhd" |> header_of_mvhd in
  let edits = Atoms.find_node_atom "edts" trak |> Atoms.find_all_leaf_atoms "elst" |> List.map edit_of_elst in
  { dur; media; edits }

let movie_from_moov moov : movie =
  let mvhd = Atoms.find_leaf_atom "mvhd" moov |> header_of_mvhd in
  let traks = Atoms.find_all_node_atoms "trak" moov |> List.map track_of_trak in
  { mvhd; traks }

let load file =
  tree := Atoms.from_file file;
  let m = Atoms.find_node_atom "moov" !tree |> movie_from_moov in
  Cutter_debug.movie m


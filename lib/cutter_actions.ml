open Cutter_data

let tree : Atoms.t list ref = ref []

let i32 bs n = Bytes.get_int32_be bs n |> Int32.to_int

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
  Cutter_debug.movie m


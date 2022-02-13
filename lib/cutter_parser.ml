open Cutter_data

let i32 bs n = Bytes.get_int32_be bs n |> Int32.to_int

let elst (mvhd : MovieDuration.t) (mdhd : MediaDuration.t) (bs : bytes) : edit list =
  let entry (idx : int) : edit =
    let p = 8 + idx * 12 in
    let dur_raw = i32 bs p in
    let dur = MovieDuration.with_value dur_raw mvhd in
    let mtime_raw = i32 bs (p + 4) in
    let mtime = MediaDuration.with_value mtime_raw mdhd in
    let mrate = (float_of_int (i32 bs (p + 8))) /. 65536.0 in
    { dur; mtime; mrate }
  in
  let rec entries = function
    | 0 -> []
    | x -> entry (x - 1) :: entries (x - 1)
  in
  i32 bs 4 |> entries |> List.rev

let mdhd (bs : bytes) : MediaDuration.t =
  let scale = i32 bs 12 |> MediaScale.of_int in
  let value = i32 bs 16 in
  { scale; value }

let mvhd (bs : bytes) : MovieDuration.t = 
  let scale = i32 bs 12 |> MovieScale.of_int in
  let value = i32 bs 16 in
  { scale; value }

let tkhd (mvhd : MovieDuration.t) (bs : bytes) : MovieDuration.t =
  let v = i32 bs 20 in
  MovieDuration.with_value v mvhd

let trak (mvhd : MovieDuration.t) (moov : Atoms.t list) : track =
  let tkhd = Atoms.find_leaf_atom "tkhd" moov |> tkhd mvhd in
  let mdhd = Atoms.find_node_atom "mdia" moov |> Atoms.find_leaf_atom "mdhd" |> mdhd in
  let edts = Atoms.find_node_atom "edts" moov |> Atoms.find_leaf_atom "elst" |> elst mvhd mdhd in
  { tkhd; mdhd; edts }

let moov (moov : Atoms.t list) : movie =
  let mvhd = Atoms.find_leaf_atom "mvhd" moov |> mvhd in
  let traks = Atoms.find_all_node_atoms "trak" moov |> List.map (trak mvhd) in
  { mvhd; traks }

let tree (tree : Atoms.t list) : movie =
  Atoms.find_node_atom "moov" tree |> moov

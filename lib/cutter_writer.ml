open Cutter_data

let i16 i n bs = Bytes.set_int16_be bs i n; bs
let i32 i n bs = Bytes.set_int32_be bs i (Int32.of_int n); bs

let mvhd (m : movie) (bs : bytes) : bytes =
  bs
  |> i32 16 m.mvhd.dur.value
  |> i16 24 m.mvhd.vol

let tkhd (t : track) (bs : bytes) : bytes =
  bs
  |> i32 20 t.tkhd.dur.value
  |> i16 36 t.tkhd.vol

let elst (e : edit list) (_ : bytes) : bytes =
  let p i = 8 + i * 12 in
  let qty = List.length e in
  let res = Bytes.make (p qty) '\x00' in
  let entry i en =
    res
    |> i32 (p i) en.dur.value
    |> i32 (4 + p i) en.mtime.value
    |> i32 (8 + p i) (Float.to_int (65536.0 *. en.mrate))
    |> ignore
  in
  i32 4 qty res |> ignore;
  List.iteri entry e;
  res

let edts (t : track) (edts : Atoms.t list) : Atoms.t list =
  Atoms.map_leaf_atom "elst" (elst t.edts) edts

let trak (t : track) (trak : Atoms.t list) : Atoms.t list =
  trak
  |> Atoms.map_leaf_atom "tkhd" (tkhd t)
  |> Atoms.map_node_atom "edts" (edts t)

let moov (m : movie) (moov : Atoms.t list) =
  moov
  |> Atoms.map_leaf_atom "mvhd" (mvhd m)
  |> Atoms.zip_node_atoms "trak" trak m.traks

let tree (tree : Atoms.t list) (m : movie) =
  Atoms.map_node_atom "moov" (moov m) tree


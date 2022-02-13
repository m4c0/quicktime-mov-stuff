open Cutter_data

let i32 bs i n = Bytes.set_int32_be bs i (Int32.of_int n); bs

let mvhd (m : movie) (mvhd : bytes) : bytes =
  i32 mvhd 16 m.mvhd.value

let tkhd (t : track) (tkhd : bytes) : bytes =
  i32 tkhd 20 t.tkhd.value

let elst (e : edit list) (_ : bytes) : bytes =
  let p i = 8 + i * 12 in
  let qty = List.length e in
  let res = Bytes.make (p qty) '\x00' in
  let entry i en =
    i32 res (p i) en.dur.value |> ignore;
    i32 res (4 + p i) en.mtime.value |> ignore;
    i32 res (8 + p i) (Float.to_int (65536.0 *. en.mrate)) |> ignore
  in
  i32 res 4 qty |> ignore;
  List.iteri entry e;
  res

let edts (t : track) (edts : Atoms.t list) : Atoms.t list =
  Atoms.map_leaf_atom "elst" (elst t.edts) edts

let trak (t : track) (trak : Atoms.t list) : Atoms.t list =
  Atoms.map_leaf_atom "tkhd" (tkhd t) trak
  |> Atoms.map_node_atom "edts" (edts t)

let moov (m : movie) (moov : Atoms.t list) =
  Atoms.map_leaf_atom "mvhd" (mvhd m) moov
  |> Atoms.zip_node_atoms "trak" trak m.traks

let tree (tree : Atoms.t list) (m : movie) =
  Atoms.map_node_atom "moov" (moov m) tree


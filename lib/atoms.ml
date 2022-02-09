type 'a node =
  | Leaf of bytes
  | Node of 'a list

type t = {
  tp : string;
  offs : int; (* TODO: automatic offset - requires some form of ordering *)
  data : t node;
}

let is_recursive = function
  | "clip"
  | "dinf"
  | "edts"
  | "imap"
  | "  in"
  | "matt"
  | "mdia"
  | "meta"
  | "minf"
  | "moov"
  | "rmda"
  | "rmra"
  | "stbl"
  | "trak"
  | "tref" -> true
  | _ -> false

let make tp bs offs : t =
  if is_recursive tp
  then { tp; offs; data = Node [] }
  else { tp; offs; data = Leaf bs }

let rec size_of (a : t) : int =
  match a.data with
  | Leaf s -> Bytes.length s
  | Node l ->
      let folder acc a = acc + (size_of a) in
      List.fold_left folder 8 l

let rec unsafe_atom_from_channel ic : t =
  let offs = Subchannel.pos_in ic in
  let sz = -8 +
    match Subchannel.input_binary_int ic with 
    | 0 -> Subchannel.limit_of ic
    | 1 -> failwith "TODO"
    | x -> x
  in
  let tp = Subchannel.input_fourcc ic in
  let lic = Subchannel.limit_by ic sz in
  let data =
    if is_recursive tp
    then Node (from_channel lic)
    else Leaf (Subchannel.input_bytes lic (Bytes.create sz) 0 sz)
  in
  Subchannel.seek_in_end lic;
  { tp; offs; data }
and atom_from_channel ic : t option =
  if Subchannel.is_empty ic
  then None
  else Some(unsafe_atom_from_channel ic)
and from_channel (ic : Subchannel.t) : t list =
  match atom_from_channel ic with
  | None -> []
  | Some(a) -> a :: (from_channel ic)

let from_file (file : string) : t list =
  Subchannel.open_with from_channel file

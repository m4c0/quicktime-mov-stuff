type t = {
  tp : string;
  offs : int;
  sz : int;
  children : t list;
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

let rec child_of ic tp : t list =
  if is_recursive tp
  then from_channel ic
  else []
and unsafe_atom_from_channel ic : t =
  let offs = Subchannel.pos_in ic in
  let sz =
    match Subchannel.input_binary_int ic with 
    | 0 -> Subchannel.limit_of ic
    | 1 -> failwith "TODO"
    | x -> x
  in
  let tp = Subchannel.input_fourcc ic in
  let lic = Subchannel.limit_by ic (sz - 8) in
  let children = child_of lic tp in
  Subchannel.seek_in_end lic;
  { tp; offs; sz; children }
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

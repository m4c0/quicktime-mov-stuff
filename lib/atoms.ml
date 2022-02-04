type t = {
  tp : string;
  offs : int;
  sz : int;
  children : t list;
}

let rec child_of ic tp : t list =
  match tp with
  | "clip"
  | "dinf"
  | "edts"
  | "imap"
  | "  in"
  | "matt"
  | "mdia"
  | "minf"
  | "moov"
  | "rmda"
  | "rmra"
  | "stbl"
  | "trak"
  | "tref" -> from_channel ic
  | _ -> []
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
  let ic = Subchannel.open_in file in
  try
    let res = from_channel ic in
    Subchannel.close_in ic;
    res
  with e ->
    Subchannel.close_in_noerr ic;
    raise e

type 'a node =
  | Leaf of bytes
  | Node of 'a list

type t = {
  tp : string;
  data : t node;
}

let leaf_of x = Leaf(x)
let node_of x = Node(x)

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

let make tp bs : t =
  if is_recursive tp
  then { tp; data = Node [] }
  else { tp; data = Leaf bs }

let rec size_of (a : t) : int =
  match a.data with
  | Leaf s -> 8 + Bytes.length s
  | Node l ->
      let folder acc a = acc + (size_of a) in
      List.fold_left folder 8 l

let rec unsafe_atom_from_channel ic : t =
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
  { tp; data }
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

let to_file (file : string) (atoms : t list) : unit =
  let oc = open_out_gen [Open_wronly; Open_creat; Open_binary] 0o666 file in
  let rec w (a : t) =
    output_binary_int oc (size_of a);
    output_string oc a.tp;
    match a.data with
    | Leaf s -> output_bytes oc s
    | Node x -> List.iter w x
  in
  try
    List.iter w atoms;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let print_csv ({ tp; data } : t) =
  match data with
  | Node _ -> print_newline ()
  | Leaf s -> Printf.printf "%s;%d\n" tp (Bytes.length s)

let print ({ tp; data } : t) =
  match data with
  | Node x -> Printf.printf "%s children:%d\n" tp (List.length x)
  | Leaf s -> Printf.printf "%s size:%d\n" tp (Bytes.length s)

let leaf_from ({ data; tp } : t) =
  match data with
  | Node _ -> failwith (tp ^ ": is not a leaf atom")
  | Leaf x -> x

let node_from ({ data; tp } : t) =
  match data with
  | Leaf _ -> failwith (tp ^ ": is not a node atom")
  | Node x -> x

let has_tp fourcc ({ tp; _ } : t) = tp = fourcc
let find_leaf_atom fourcc (l : t list) = List.find (has_tp fourcc) l |> leaf_from
let find_node_atom fourcc (l : t list) = List.find (has_tp fourcc) l |> node_from
let find_all_leaf_atoms fourcc (l : t list) = List.find_all (has_tp fourcc) l |> List.map leaf_from
let find_all_node_atoms fourcc (l : t list) = List.find_all (has_tp fourcc) l |> List.map node_from

let map_atom fourcc fn (l : t list) =
  let mapper a = if has_tp fourcc a then { a with data = fn a } else a in
  List.map mapper l

let map_leaf_atom fourcc (fn : bytes -> bytes) =
  let f x = x |> leaf_from |> fn |> leaf_of in
  map_atom fourcc f

let map_node_atom fourcc (fn : t list -> t list) =
  let f x = x |> node_from |> fn |> node_of in
  map_atom fourcc f

let rec map_atoms fourcc fn (vl : 'a list) (al : t list) : t list =
  match vl, al with
  | [], [] -> []
  | _, ({ tp; _ } as atom) :: ll when tp <> fourcc -> atom :: map_atoms fourcc fn vl ll
  | v :: vs, ({ tp; _ } as a) :: ll when tp = fourcc -> 
      let atom = { tp; data = fn v a } in
      atom :: map_atoms fourcc fn vs ll
  | _, _ -> failwith "mismatched atoms"

let zip_leaf_atoms fourcc (fn : 'a -> bytes -> bytes) (aa : 'a list) =
  let f v a = a |> leaf_from |> fn v |> leaf_of in
  map_atoms fourcc f aa

let zip_node_atoms fourcc (fn : 'a -> t list -> t list) (aa : 'a list) =
  let f v a = a |> node_from |> fn v |> node_of in
  map_atoms fourcc f aa


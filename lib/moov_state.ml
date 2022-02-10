let atom_tree : Atoms.t list ref = ref []
let cursor : int list ref = ref [ 0 ]

let rec atom_at l =
  match l with
  | [] -> failwith "cursor is deeper than the atom tree"
  | [i] -> List.nth !atom_tree i
  | i :: ll ->
      match atom_at ll with
      | { data = Leaf _; _ } -> failwith "cursor tries to enter a leaf atom"
      | { data = Node n; _ } -> List.nth n i

let atom_at_cursor () = atom_at !cursor

let map_atom_at_cursor (fn : Atoms.t -> Atoms.t) =
  let rec rfn cl idx a =
    match cl with
    | [] -> failwith "cursor is deeper than the atom tree"
    | [i] when i == idx -> fn a
    | i :: ll when i == idx -> (
        match a.data with
        | Leaf _ -> failwith "cursor tries to enter a leaf atom"
        | Node n -> { a with data = Node (List.mapi (rfn ll) n) }
    )
    | _ -> a
  in
  let rcur = List.rev !cursor in
  atom_tree := List.mapi (rfn rcur) !atom_tree

let move_cursor idx =
  let nc = idx :: (List.tl !cursor) in
  try
    let a = atom_at nc in
    cursor := nc;
    a
  with e -> raise e

let fold_tree fn init = List.fold_left fn init !atom_tree
let iter_tree fn = List.iter fn !atom_tree
let map_tree fn = atom_tree := fn !atom_tree; !atom_tree

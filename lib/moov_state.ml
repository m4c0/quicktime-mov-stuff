let atom_tree : Atoms.t list ref = ref []
let cursor : int ref = ref 0

let atom_at_opt o : Atoms.t option =
  let offs_match (a : Atoms.t) = a.offs == o in
  List.find_opt offs_match !atom_tree

let atom_at_cursor () =
  match atom_at_opt !cursor with
  | None -> failwith "Cursor at invalid position"
  | Some a -> a

let map_atom_at_cursor fn =
  let repl (a : Atoms.t) =
    if !cursor = a.offs
    then fn a
    else a
  in
  atom_tree := List.map repl !atom_tree

let move_cursor o =
  match atom_at_opt o with
  | None -> failwith (Printf.sprintf "%d: no atom there" o)
  | Some a -> cursor := o; a

let fold_tree fn init = List.fold_left fn init !atom_tree
let iter_tree fn = List.iter fn !atom_tree
let map_tree fn = atom_tree := fn !atom_tree; !atom_tree

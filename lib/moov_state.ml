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

let file_size () =
  let fold res ({ offs; sz; _ } : Atoms.t) = max res (offs + sz) in
  List.fold_left fold 0 !atom_tree

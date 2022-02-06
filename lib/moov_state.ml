let atom_tree : Atoms.t list ref = ref []
let cursor : int ref = ref 0

let atom_at_opt o : Atoms.t option =
  let offs_match ({ offs; _ } : Atoms.t) = offs = o in
  List.find_opt offs_match !atom_tree


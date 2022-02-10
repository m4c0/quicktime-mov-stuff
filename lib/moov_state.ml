let atom_tree : Atoms.t list ref = ref []
let cursor : int ref = ref 0

let atom_at_opt o : Atoms.t option =
  let rec fn offs (l : Atoms.t list) =
    match l with
    | [] -> None
    | a :: _ when o == offs -> Some a
    | { data = Leaf b; _ } :: ll -> fn (offs + 8 + (Bytes.length b)) ll
    | { data = Node n; _ } :: ll -> fn (offs + 8) (n @ ll)
  in
  fn 0 !atom_tree

let atom_at_cursor () =
  match atom_at_opt !cursor with
  | None -> failwith "Cursor at invalid position"
  | Some a -> a

let map_atom_at_cursor (fn : Atoms.t -> Atoms.t) =
  let rec f (offs : int) (l : Atoms.t list) =
    match l with
    | [] -> []
    | a :: ll when !cursor == offs -> (fn a) :: ll
    | { data = Leaf b; _ } as a :: ll -> a :: f (offs + 8 + (Bytes.length b)) ll
    | { data = Node n; _ } as a :: ll -> a :: f (offs + 8) (n @ ll)
  in
  atom_tree := f 0 !atom_tree

let move_cursor o =
  match atom_at_opt o with
  | None -> failwith (Printf.sprintf "%d: no atom there" o)
  | Some a -> cursor := o; a

let fold_tree fn init = List.fold_left fn init !atom_tree
let iter_tree fn = List.iter fn !atom_tree
let map_tree fn = atom_tree := fn !atom_tree; !atom_tree

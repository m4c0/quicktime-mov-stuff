type move_type =
  | Absolute of int
  | Relative of int
  | Inwards
  | Outwards
  
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

let move_cursor (mt : move_type) =
  let hd = List.hd !cursor in
  let tl = List.tl !cursor in
  let nc = 
    match mt with
    | Absolute idx -> idx :: tl
    | Relative idx -> (hd + idx) :: tl
    | Inwards -> 0 :: !cursor
    | Outwards -> List.tl !cursor
  in
  try
    let a = atom_at nc in
    cursor := nc;
    a
  with e -> raise e

let rec find (fourcc : string) (tree : Atoms.t list) (cur : int list) =
  let hd = List.hd cur in
  let tl = List.tl cur in
  match tree with
  | { tp; _ } :: _ when tp = fourcc -> cur
  | { data=Node(n); _ } :: tt -> (
      try find fourcc n (0 :: cur)
      with _ -> find fourcc tt (hd + 1 :: tl)
  )
  | { data=Leaf(_); _ } :: tt -> 
      find fourcc tt (hd + 1 :: tl)
  | [] -> failwith (fourcc ^ ": not found")

let find_first (fourcc : string) =
  cursor := find fourcc !atom_tree [0];
  atom_at_cursor ()

let find_next (fourcc : string) =
  let rec sub n l = if n = -1 then l else sub (n - 1) (List.tl l) in
  let rec bs atoms cur ncur = match cur with
  | [] -> failwith (fourcc ^ ": not found")
  | [x] -> find fourcc (sub x atoms) [x + 1]
  | hd :: tl ->
      try bs (List.nth atoms hd |> Atoms.node_from) tl (hd :: ncur) 
      with _ -> find fourcc (sub hd atoms) (hd + 1 :: ncur)
  in
  cursor := bs !atom_tree (List.rev !cursor) [];
  atom_at_cursor ()

let fold_tree fn init = List.fold_left fn init !atom_tree
let iter_tree fn = List.iter fn !atom_tree
let map_tree fn = atom_tree := fn !atom_tree; !atom_tree

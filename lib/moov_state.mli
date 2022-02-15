type move_type =
  | Absolute of int
  | Relative of int
  | Inwards
  | Outwards
  
val atom_at_cursor : unit -> Atoms.t
val map_atom_at_cursor : (Atoms.t -> Atoms.t) -> unit 
val move_cursor : move_type -> Atoms.t

val find_first : string -> Atoms.t
val find_next : string -> Atoms.t

val fold_tree : ('a -> Atoms.t -> 'a) -> 'a -> 'a
val iter_tree : (Atoms.t -> unit) -> unit 
val map_tree : (Atoms.t list -> Atoms.t list) -> Atoms.t list

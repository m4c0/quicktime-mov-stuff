val atom_at_opt : int -> Atoms.t option

val atom_at_cursor : unit -> Atoms.t
val map_atom_at_cursor : (Atoms.t -> Atoms.t) -> unit 
val move_cursor : int -> Atoms.t

val fold_tree : ('a -> Atoms.t -> 'a) -> 'a -> 'a
val iter_tree : (Atoms.t -> unit) -> unit 
val map_tree : (Atoms.t list -> Atoms.t list) -> Atoms.t list

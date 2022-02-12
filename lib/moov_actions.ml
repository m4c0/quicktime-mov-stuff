let current_file : string ref = ref "a.mov"
let printer : (Atoms.t -> unit) ref = ref Atoms.print_csv

let new_kid_on_the_block tp bs l = List.append l [Atoms.make tp bs]
let move mt = Moov_state.move_cursor mt |> !printer

(* *)

let append tp bs =
  Moov_state.map_tree (new_kid_on_the_block tp bs)
  |> List.rev
  |> List.hd
  |> !printer

let append_children tp bs =
  let nkotb (a : Atoms.t) : Atoms.t Atoms.node =
    match a.data with
    | Leaf _ -> failwith "can't add children to leaf atoms"
    | Node l -> Node (new_kid_on_the_block tp bs l)
  in
  let mapper (a : Atoms.t) = { a with data = nkotb a } in
  let _ = Moov_state.map_atom_at_cursor mapper in
  let ({ data; _ } : Atoms.t) = Moov_state.atom_at_cursor () in
  match data with
  | Leaf _ -> failwith "this should neve happen"
  | Node l -> l |> List.rev |> List.hd |> !printer

let dump () =
  let a = Moov_state.atom_at_cursor() in
  let fn = Moov_atom_parsers.parser_of a.tp in
  match a.data with
  | Node _ -> failwith (a.tp ^ ": can't dump atoms with children")
  | Leaf b -> fn b

let edit file =
  let new_list = Moov_state.map_tree (fun _ -> Atoms.from_file file) in
  let len = List.length new_list in
  if len > 0 then Moov_state.move_cursor (Absolute 0) |> ignore;
  current_file := file;
  Printf.printf "%d\n" len

let jump offs = move (Absolute offs)

let print () = 
  Moov_state.atom_at_cursor () |> !printer 

let print_children () =
  let a = Moov_state.atom_at_cursor () in
  match a.data with
  | Leaf _ -> failwith (a.tp ^ ": does not contain children")
  | Node x -> List.iter !printer x

let print_roots () = Moov_state.iter_tree !printer

let print_tree () =
  let rec r i (a : Atoms.t) =
    print_string i;
    !printer a;
    match a.data with
    | Leaf _ -> ()
    | Node x -> List.iter (r ("     " ^ i)) x
  in
  Moov_state.iter_tree (r "")

let replace_size len =
  let fn (a : Atoms.t) : Atoms.t =
    match a.data with
    | Leaf _ -> { a with data = Leaf len }
    | Node _ -> failwith (a.tp ^ ": size is defined by its children")
  in
  Moov_state.map_atom_at_cursor fn;
  Moov_state.atom_at_cursor () |> !printer 

let replace_type fourcc =
  Moov_state.map_atom_at_cursor (fun a -> { a with tp = fourcc });
  Moov_state.atom_at_cursor () |> !printer 

let step () = move (Relative 1)
let step_back () = move (Relative (-1))
let step_in () = move Inwards 
let step_out () = move Outwards 

let write_copy file =
  let f atoms =
    Atoms.to_file file atoms;
    atoms
  in
  Moov_state.map_tree f |> ignore

let write () = write_copy !current_file

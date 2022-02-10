type target = Machine | Human

let current_file : string ref = ref "a.mov"
let printer : (Atoms.t -> unit) ref = ref Atoms.print_csv

let print_single _ (a : Atoms.t) = !printer a

let new_kid_on_the_block tp bs i l =
  let max_len res (a : Atoms.t) = max res (a.offs + Atoms.size_of a) in
  let offs = List.fold_left max_len i l in
  let atom : Atoms.t = Atoms.make tp bs offs in
  atom :: l

(* *)

let append fmt tp bs =
  Moov_state.map_tree (new_kid_on_the_block tp bs 0)
  |> List.hd
  |> print_single fmt

let append_children fmt tp bs =
  let nkotb (a : Atoms.t) : Atoms.t Atoms.node =
    match a.data with
    | Leaf _ -> failwith "can't add children to leaf atoms"
    | Node l -> Node (new_kid_on_the_block tp bs (a.offs + 8) l)
  in
  let mapper (a : Atoms.t) = { a with data = nkotb a } in
  let _ = Moov_state.map_atom_at_cursor mapper in
  let ({ data; _ } : Atoms.t) = Moov_state.atom_at_cursor () in
  match data with
  | Leaf _ -> failwith "this should neve happen"
  | Node l -> l |> List.hd |> print_single fmt

let dump () =
  let a = Moov_state.atom_at_cursor() in
  let fn = Moov_atom_parsers.parser_of a.tp in
  match a.data with
  | Node _ -> failwith (a.tp ^ ": can't dump atoms with children")
  | Leaf b -> fn b

let edit file =
  let new_list = Moov_state.map_tree (fun _ -> Atoms.from_file file) in
  let len = List.length new_list in
  if len > 0 then Moov_state.move_cursor 0 |> ignore;
  current_file := file;
  Printf.printf "%d\n" len

let jump fmt offs = Moov_state.move_cursor offs |> print_single fmt

let print fmt = 
  Moov_state.atom_at_cursor () |> print_single fmt 

let print_children fmt =
  let a = Moov_state.atom_at_cursor () in
  match a.data with
  | Leaf _ -> failwith (a.tp ^ ": does not contain children")
  | Node x -> List.iter (print_single fmt) x

let print_roots fmt = Moov_state.iter_tree (print_single fmt)

let print_tree () =
  let rec r i (a : Atoms.t) =
    print_string i;
    print_single Human a;
    match a.data with
    | Leaf _ -> ()
    | Node x -> List.iter (r ("     " ^ i)) x
  in
  Moov_state.iter_tree (r "")

let replace_size fmt len =
  let fn (a : Atoms.t) : Atoms.t =
    match a.data with
    | Leaf _ -> { a with data = Leaf len }
    | Node _ -> failwith (a.tp ^ ": size is defined by its children")
  in
  Moov_state.map_atom_at_cursor fn;
  Moov_state.atom_at_cursor () |> print_single fmt 

let replace_type fmt fourcc =
  Moov_state.map_atom_at_cursor (fun a -> { a with tp = fourcc });
  Moov_state.atom_at_cursor () |> print_single fmt 

let sort () =
  let by_offs ({ offs = oa; _ } : Atoms.t) ({ offs = ob; _ } : Atoms.t) = compare oa ob; in
  let rec sort_kids (a : Atoms.t) =
    match a.data with
    | Leaf _ -> a
    | Node x -> { a with data = Node (rec_sort x) }
  and rec_sort a : Atoms.t list = a |> List.sort by_offs |> List.map sort_kids in
  Moov_state.map_tree rec_sort |> ignore

let verify () =
  let rec checker pos (a : Atoms.t) =
    if pos > a.offs
    then failwith (Printf.sprintf "expecting offset %d for %s but got %d" pos a.tp a.offs);

    match a.data with
    | Node children ->
      if children = [] then failwith (Printf.sprintf "expecting children at %d for %s" pos a.tp);
      List.fold_left checker 0 children
    | Leaf s -> a.offs + (Bytes.length s)

  in
  let file_size = try (Unix.stat !current_file).st_size with _ -> 0 in
  let tree_size = Moov_state.fold_tree checker 0 in
  if file_size = tree_size
  then Printf.printf "file size is %d\n" file_size
  else if file_size < tree_size
  then Printf.printf "file size will grow - disk: %d - tree: %d\n" file_size tree_size
  else Printf.printf "atoms are not covering the whole file - disk: %d - tree %d\n" file_size tree_size

let write_copy file =
  let oc = open_out_gen [Open_wronly; Open_creat; Open_binary] 0o666 file in
  let rec w (a : Atoms.t) =
    seek_out oc a.offs;
    output_binary_int oc (Atoms.size_of a);
    output_string oc a.tp;
    match a.data with
    | Leaf s ->
        while (pos_out oc < (a.offs + (Bytes.length s))) do
          output_byte oc 0
        done
    | Node x -> List.iter w x
  in
  try
    Moov_state.iter_tree w;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let write () = write_copy !current_file

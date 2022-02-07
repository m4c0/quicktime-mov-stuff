type target = Machine | Human
let print_single (tgt : target) ({ tp; offs; sz; children } : Atoms.t) =
  match tgt with
  | Machine -> Printf.printf "%s;%d;%d;%d\n" tp offs sz (List.length children)
  | Human -> Printf.printf "%s @%d size:%d (%d children)\n" tp offs sz (List.length children)

(* *)

let append tp sz =
  let max_len res ({ offs; sz; _ } : Atoms.t) = max res (offs + sz) in
  let offs = Moov_state.fold_tree max_len 0 in
  let atom : Atoms.t = { tp; offs; sz; children = [] } in
  Moov_state.map_tree (fun l -> atom :: l) |> ignore;
  print_int offs;
  print_newline ()

let edit file =
  Moov_state.map_tree (fun _ -> Atoms.from_file file)
  |> List.length |> print_int;
  print_newline ()

let jump fmt offs = Moov_state.move_cursor offs |> print_single fmt

let print fmt = Moov_state.iter_tree (print_single fmt)

let replace_size fmt len =
  Moov_state.map_atom_at_cursor (fun a -> { a with sz = len });
  Moov_state.atom_at_cursor () |> print_single fmt 

let replace_type fmt fourcc =
  Moov_state.map_atom_at_cursor (fun a -> { a with tp = fourcc });
  Moov_state.atom_at_cursor () |> print_single fmt 

let sort () =
  let by_offs ({ offs = oa; _ } : Atoms.t) ({ offs = ob; _ } : Atoms.t) = compare oa ob; in
  Moov_state.map_tree (List.sort by_offs) |> ignore

let verify () =
  let checker pos ({ tp; offs; sz; _ } : Atoms.t) =
    if pos > offs then failwith (Printf.sprintf "expecting offset %d for %s but got %d" pos tp offs)
    else offs + sz
  in
  Moov_state.fold_tree checker 0
  |> Printf.printf "total file is %d\n"

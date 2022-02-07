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
  try
    Moov_state.map_tree (fun _ -> Atoms.from_file file)
    |> List.length |> print_int;
    print_newline ()
  with Sys_error e -> print_endline e

let jump fmt str = 
  match int_of_string_opt str with
  | None -> print_endline (str ^ ": invalid offset")
  | Some o -> Moov_state.move_cursor o |> print_single fmt

let print fmt = Moov_state.iter_tree (print_single fmt)

let replace_size fmt len_str =
  try 
    let len = int_of_string len_str in
    Moov_state.map_atom_at_cursor (fun a -> { a with sz = len });
    Moov_state.atom_at_cursor () |> print_single fmt 
  with Failure f -> print_endline f

let replace_type fmt fourcc =
  Moov_state.map_atom_at_cursor (fun a -> { a with tp = fourcc });
  try Moov_state.atom_at_cursor () |> print_single fmt 
  with Failure f -> print_endline f

let sort () =
  let by_offs ({ offs = oa; _ } : Atoms.t) ({ offs = ob; _ } : Atoms.t) = compare oa ob; in
  Moov_state.map_tree (List.sort by_offs) |> ignore

let verify () =
  let checker pos ({ tp; offs; sz; _ } : Atoms.t) =
    if pos > offs then failwith (Printf.sprintf "expecting offset %d for %s but got %d" pos tp offs)
    else offs + sz
  in
  try
    Moov_state.fold_tree checker 0
    |> Printf.printf "total file is %d\n"
  with Failure f -> print_endline f

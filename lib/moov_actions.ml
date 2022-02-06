type target = Machine | Human
let print_single (tgt : target) ({ tp; offs; sz; children } : Atoms.t) =
  match tgt with
  | Machine -> Printf.printf "%s;%d;%d;%d\n" tp offs sz (List.length children)
  | Human -> Printf.printf "%s @%d size:%d (%d children)\n" tp offs sz (List.length children)

(* *)

let rec append () =
  match read_line () with
  | "." -> ()
  | line ->
      let append_item tp offs sz =
        let atom : Atoms.t = { tp; offs; sz; children = [] } in
        Moov_state.atom_tree := atom :: !Moov_state.atom_tree
      in
      try
        Scanf.sscanf line "%4s;%d;%d" append_item;
        append ()
      with _ -> print_endline "?"

let edit file =
  try
    Moov_state.atom_tree := Atoms.from_file file;
    !Moov_state.atom_tree |> List.length |> print_int;
    print_newline ()
  with Sys_error e -> print_endline e

let jump fmt str = 
  match int_of_string_opt str with
  | None -> print_endline (str ^ ": invalid offset")
  | Some o ->
      match Moov_state.atom_at_opt o with
      | None -> print_endline (str ^ ": no atom there")
      | Some a -> Moov_state.cursor := o; print_single fmt a

let print fmt =
  List.iter (print_single fmt) !Moov_state.atom_tree

let sort () =
  let by_offs ({ offs = oa; _ } : Atoms.t) ({ offs = ob; _ } : Atoms.t) = compare oa ob; in
  Moov_state.atom_tree := List.sort by_offs !Moov_state.atom_tree

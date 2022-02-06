let break_str_at str idx =
  let left = String.sub str 0 idx in
  let right = String.sub str (idx + 1) (String.length str - idx - 1) in
  (left, right)

let break_first_word str =
  String.index_opt str ' '
  |> Option.map (break_str_at str)
  |> Option.value ~default:(str, "")

let atom_tree : Atoms.t list ref = ref []

let load file =
  try
    atom_tree := Atoms.from_file file;
    Printf.printf "loaded %d atoms from '%s'\n" (List.length !atom_tree) file
  with Sys_error e -> print_endline e

let print () =
  let print_single ({ tp; offs; sz; children } : Atoms.t) =
    Printf.printf "%s @%d size:%d (%d children)\n" tp offs sz (List.length children)
  in
  List.iter print_single !atom_tree

let run cmd =
  match break_first_word cmd with
  | ("load", "") -> print_endline "Missing file name"
  | ("load", file) -> load file
  | ("print", "") -> print ()
  | _ -> print_endline "No hablo su lingua"

let rec repl () =
  try
    read_line () |> run;
    repl ()
  with End_of_file -> ()


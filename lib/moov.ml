let atom_tree : Atoms.t list ref = ref []

let edit file =
  try
    atom_tree := Atoms.from_file file;
    !atom_tree |> List.length |> print_int;
    print_newline ()
  with Sys_error e -> print_endline e

let print () =
  let print_single ({ tp; offs; sz; children } : Atoms.t) =
    Printf.printf "%s @%d size:%d (%d children)\n" tp offs sz (List.length children)
  in
  List.iter print_single !atom_tree

let range_cmd cmd =
  match Stringy.break_first_word cmd with
  | ("p", "") -> print ()
  | _ -> print_endline "Je ne parle pas cela"

let run cmd =
  match Stringy.break_first_word cmd with
  | ("e", "") -> print_endline "Missing file name"
  | ("e", file) -> edit file
  | ("%", "") -> print_endline "Missing command after range"
  | ("%", cmd) -> range_cmd cmd
  | _ -> print_endline "No hablo su lingua"

let rec repl () =
  try
    read_line () |> run;
    repl ()
  with End_of_file -> ()


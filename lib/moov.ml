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

let list_of_str s = List.init (String.length s) (String.get s)
let trimmed_substr s idx = 
  String.sub s idx ((String.length s) - idx)
  |> String.trim

let run (str : string) =
  match list_of_str str with
  | 'e' :: ' ' :: _ -> edit (trimmed_substr str 2)
  | ['%'; 'p']
  | ['%'; ' '; 'p'] -> print ()
  | _ -> print_endline "?"

let rec repl () =
  try
    read_line () |> run;
    repl ()
  with End_of_file -> ()


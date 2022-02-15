let list_of_str s = List.init (String.length s) (String.get s)
let trimmed_substr s idx = 
  String.sub s idx ((String.length s) - idx)
  |> String.trim

let fourcc a b c d =
  [a; b; c; d] |> List.map (String.make 1) |> String.concat ""

let ios (msg : string) (str : string) : int =
  try int_of_string str
  with _ -> failwith (str ^ ": " ^ msg)

let run (str : string) =
  let open Moov_actions in
  let trim = trimmed_substr str in
  let offs i = trim i |> ios "invalid offset" in
  let size i = trim i |> ios "invalid size" in
  let bc = Bytes.create in
  match list_of_str str with
  | 'a' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append (fourcc a b c d) (offs 7 |> bc)
  | 'a' :: 'c' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append_children (fourcc a b c d) (offs 8 |> bc)
  | ['d'] -> dump ()
  | 'e' :: ' ' :: _ -> edit (trim 2)
  | ['f'; ' '; a; b; c; d] -> find_first (fourcc a b c d)
  | ['f'; 'n'; ' '; a; b; c; d] -> find_next (fourcc a b c d)
  | 'j' :: ' ' :: _ -> jump (offs 2)
  | 'J' :: ' ' :: _ -> jump (offs 2)
  | ['p'] -> print ()
  | ['p'; 'c'] -> print_children ()
  | ['p'; 'r'] -> print_roots ()
  | ['p'; 't'] -> print_tree ()
  | 'r' :: 's' :: ' ' :: _ -> replace_size (size 3 |> bc)
  | ['r'; 't'; ' '; a; b; c; d] -> replace_type (fourcc a b c d)
  | ['s'] -> step ()
  | ['S'] -> step_back ()
  | ['s'; 'i'] -> step_in ()
  | ['s'; 'o'] -> step_out ()
  | ['w'] -> write ()
  | 'w' :: ' ' :: _ -> write_copy (trim 2)
  | _ -> print_endline "?"

let repl () =
  let batch = ref false in
  let speclist = [
    ("-b", Arg.Set batch, "Batch mode - emits all output in a computer-friendly way")
  ] in
  let anon_fn x = failwith ("Unknown argument: " ^ x) in
  let usage_msg = Sys.argv.(0) ^ " [-b]" in
  Arg.parse speclist anon_fn usage_msg;
  Moov_actions.printer := if !batch then Atoms.print_csv else Atoms.print;
  Repl.repl run;


let list_of_str s = List.init (String.length s) (String.get s)
let trimmed_substr s idx = 
  String.sub s idx ((String.length s) - idx)
  |> String.trim

let fourcc a b c d =
  [a; b; c; d] |> List.map (String.make 1) |> String.concat ""

let ios (msg : string) (str : string) : int =
  try int_of_string str
  with _ -> failwith (str ^ ": " ^ msg)

open Moov_actions
let run (str : string) =
  let trim = trimmed_substr str in
  let offs i = trim i |> ios "invalid offset" in
  let size i = trim i |> ios "invalid size" in
  let bc = Bytes.create in
  match list_of_str str with
  | 'a' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append (fourcc a b c d) (offs 7 |> bc)
  | 'a' :: 'c' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append_children (fourcc a b c d) (offs 8 |> bc)
  | ['d'] -> dump ()
  | 'e' :: ' ' :: _ -> edit (trim 2)
  | 'j' :: ' ' :: _ -> jump (offs 2)
  | 'J' :: ' ' :: _ -> jump (offs 2)
  | ['p'] -> print ()
  | ['p'; 'c'] -> print_children ()
  | ['p'; 'r'] -> print_roots ()
  | ['p'; 't'] -> print_tree ()
  | 'r' :: 's' :: ' ' :: _ -> replace_size (size 3 |> bc)
  | ['r'; 't'; ' '; a; b; c; d] -> replace_type (fourcc a b c d)
  | ['w'] -> write ()
  | 'w' :: ' ' :: _ -> write_copy (trim 2)
  | _ -> print_endline "?"

let safe_run (str : string) =
  try run str
  with
  | Failure f
  | Invalid_argument f
  | Sys_error f -> print_endline f
  | x -> print_endline ("Unhandled error: " ^ (Printexc.to_string x))
  
let read_line_opt () =
  try Some (read_line ())
  with End_of_file -> None 

let repl () =
  let rec loop () =
    match read_line_opt () with
    | None -> ()
    | Some line -> (safe_run line; loop())
  in
  let batch = ref false in
  let speclist = [
    ("-b", Arg.Set batch, "Batch mode - emits all output in a computer-friendly way")
  ] in
  let anon_fn x = failwith ("Unknown argument: " ^ x) in
  let usage_msg = Sys.argv.(0) ^ " [-b]" in
  Arg.parse speclist anon_fn usage_msg;
  Moov_actions.printer := if !batch then Atoms.print_csv else Atoms.print;
  loop ();


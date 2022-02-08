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
  match list_of_str str with
  | 'a' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append Machine (fourcc a b c d) (offs 7)
  | 'A' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append Human (fourcc a b c d) (offs 7)
  | 'a' :: 'c' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append_children Machine (fourcc a b c d) (offs 8)
  | 'A' :: 'C' :: ' ' :: a :: b :: c :: d :: ' ' :: _ -> append_children Human (fourcc a b c d) (offs 8)
  | ['D'] -> dump ()
  | ['D'; 'X'] -> dump_hex ()
  | 'e' :: ' ' :: _ -> edit (trim 2)
  | 'j' :: ' ' :: _ -> jump Machine (offs 2)
  | 'J' :: ' ' :: _ -> jump Human (offs 2)
  | ['p'] -> print Machine
  | ['P'] -> print Human
  | ['p'; 'c'] -> print_children Machine
  | ['P'; 'C'] -> print_children Human
  | ['p'; 'r'] -> print_roots Machine
  | ['P'; 'R'] -> print_roots Human
  | 'r' :: 's' :: ' ' :: _ -> replace_size Machine (size 3)
  | 'R' :: 'S' :: ' ' :: _ -> replace_size Human (size 3)
  | ['r'; 't'; ' '; a; b; c; d] -> replace_type Machine (fourcc a b c d)
  | ['R'; 'T'; ' '; a; b; c; d] -> replace_type Human (fourcc a b c d)
  | ['s'] -> sort ()
  | ['V'] -> verify ()
  | ['w'] -> write ()
  | 'w' :: ' ' :: _ -> write_copy (trim 2)
  | ['W'] -> verify (); write ()
  | 'W' :: ' ' :: _ -> verify (); write_copy (trim 2)
  | _ -> print_endline "?"

let safe_run (str : string) =
  try run str
  with
  | Failure f
  | Sys_error f -> print_endline f

let rec repl () =
  try
    read_line () |> safe_run;
    repl ()
  with End_of_file -> ()


let list_of_str s = List.init (String.length s) (String.get s)
let trimmed_substr s idx = 
  String.sub s idx ((String.length s) - idx)
  |> String.trim

open Moov_actions
let run (str : string) =
  match list_of_str str with
  | ['a'] -> append ()
  | 'e' :: ' ' :: _ -> edit (trimmed_substr str 2)
  | 'j' :: ' ' :: _ -> jump Machine (trimmed_substr str 2)
  | 'J' :: ' ' :: _ -> jump Human (trimmed_substr str 2)
  | ['p'] -> print Machine
  | ['P'] -> print Human
  | ['s'] -> sort ()
  | _ -> print_endline "?"

let rec repl () =
  try
    read_line () |> run;
    repl ()
  with End_of_file -> ()


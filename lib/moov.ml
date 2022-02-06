let list_of_str s = List.init (String.length s) (String.get s)
let trimmed_substr s idx = 
  String.sub s idx ((String.length s) - idx)
  |> String.trim

let fourcc a b c d =
  [a; b; c; d] |> List.map (String.make 1) |> String.concat ""

open Moov_actions
let run (str : string) =
  match list_of_str str with
  | ['a'] -> append ()
  | 'e' :: ' ' :: _ -> edit (trimmed_substr str 2)
  | 'j' :: ' ' :: _ -> jump Machine (trimmed_substr str 2)
  | 'J' :: ' ' :: _ -> jump Human (trimmed_substr str 2)
  | ['p'] -> print Machine
  | ['P'] -> print Human
  | ['r'; 't'; ' '; a; b; c; d] -> replace_type Machine (fourcc a b c d)
  | ['R'; 'T'; ' '; a; b; c; d] -> replace_type Human (fourcc a b c d)
  | ['s'] -> sort ()
  | _ -> print_endline "?"

let rec repl () =
  try
    read_line () |> run;
    repl ()
  with End_of_file -> ()


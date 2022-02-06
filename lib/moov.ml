let list_of_str s = List.init (String.length s) (String.get s)
let trimmed_substr s idx = 
  String.sub s idx ((String.length s) - idx)
  |> String.trim

let fourcc a b c d =
  [a; b; c; d] |> List.map (String.make 1) |> String.concat ""

open Moov_actions
let run (str : string) =
  let trim = trimmed_substr str in
  match list_of_str str with
  | ['a'] -> append ()
  | 'e' :: ' ' :: _ -> edit (trim 2)
  | 'j' :: ' ' :: _ -> jump Machine (trim 2)
  | 'J' :: ' ' :: _ -> jump Human (trim 2)
  | ['p'] -> print Machine
  | ['P'] -> print Human
  | 'r' :: 's' :: ' ' :: _ -> replace_size Machine (trim 3)
  | 'R' :: 'S' :: ' ' :: _ -> replace_size Human (trim 3)
  | ['r'; 't'; ' '; a; b; c; d] -> replace_type Machine (fourcc a b c d)
  | ['R'; 'T'; ' '; a; b; c; d] -> replace_type Human (fourcc a b c d)
  | ['s'] -> sort ()
  | _ -> print_endline "?"

let rec repl () =
  try
    read_line () |> run;
    repl ()
  with End_of_file -> ()


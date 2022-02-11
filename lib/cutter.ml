let spliteroo str =
  match String.index_opt str ' ' with
  | None -> (str, "")
  | Some i ->
      let len = String.length str in
      (String.sub str 0 i, String.sub str (i + 1) (len - i - 1))

open Cutter_actions
let run str =
  match spliteroo str with
  | ("load", file) -> load file
  | (_, _) -> print_endline "?"

let repl () = Repl.repl run

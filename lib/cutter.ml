let spliteroo str =
  match String.index_opt str ' ' with
  | None -> (str, "")
  | Some i ->
      let len = String.length str in
      (String.sub str 0 i, String.sub str (i + 1) (len - i - 1))

let run str =
  let open Cutter_actions in
  let trim fn x =
    match String.split_on_char ' ' x with
    | [t; e; d] ->
        let ti = int_of_string t in
        let ei = int_of_string e in
        let di = int_of_string d in
        fn ti ei di
    | _ -> failwith "invalid format for 'add'"
  in
  match spliteroo str with
  | ("load", file) -> load file
  | ("ltrim", x) -> trim ltrim x
  | ("rtrim", x) -> trim rtrim x
  | ("open", "") -> play ()
  | (x, _) -> failwith (x ^ ": unknown command")

let repl () = Repl.repl run

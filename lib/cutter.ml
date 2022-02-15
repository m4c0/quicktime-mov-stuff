let spliteroo str =
  match String.index_opt str ' ' with
  | None -> (str, "")
  | Some i ->
      let len = String.length str in
      (String.sub str 0 i, String.sub str (i + 1) (len - i - 1))

let run str =
  let open Cutter_actions in
  let snikt fn x =
    match String.split_on_char ' ' x with
    | ["*"; e; d] ->
        let ei = int_of_string e in
        let di = int_of_string d in
        foreach_track (fun t -> fn t ei di)
    | [t; e; d] ->
        let ti = int_of_string t in
        let ei = int_of_string e in
        let di = int_of_string d in
        fn ti ei di
    | _ -> failwith "invalid format for trimming"
  in
  let snikt2 fn x =
    match String.split_on_char ' ' x with
    | ["*"; e] ->
        let ei = int_of_string e in
        foreach_track (fun t -> fn t ei)
    | [t; e] ->
        let ti = int_of_string t in
        let ei = int_of_string e in
        fn ti ei
    | _ -> failwith "invalid format for trimming"
  in
  match spliteroo str with
  | ("load", file) -> load file
  | ("trackvolume", x) -> snikt2 volume x
  | ("volume", x) -> movie_volume (int_of_string x)
  | ("delete", x) -> snikt2 delete x
  | ("ltrim", x) -> snikt ltrim x
  | ("rtrim", x) -> snikt rtrim x
  | ("split", x) -> snikt split x
  | ("open", "") -> play ()
  | ("saveas", x) -> save_as x
  | ("dump", "") -> dump ()
  | ("undo", "") -> undo ()
  | ("redo", "") -> redo ()
  | (x, _) -> failwith (x ^ ": unknown command")

let repl () = Repl.repl run

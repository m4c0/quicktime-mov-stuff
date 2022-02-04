let chunk_like_table_skip = function
  | "dref" -> Some(8)
  | "stsd" -> Some(8)
  | _ -> None

let rec dump_atom indent ({ tp; sz; children; _ } : QTFF.Atoms.t) =
  let ni = "     " ^ indent in
  let r = dump_atom ni in
  Printf.printf "%s%s -- %d bytes\n" indent tp sz;
  List.iter r children
  
let dump file =
  QTFF.Atoms.from_file file
    |> List.iter (dump_atom "")

let () =
  match Sys.argv with
  | [| _; file |] -> dump file
  | _ -> print_endline "Missing file name"

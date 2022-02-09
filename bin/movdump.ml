let chunk_like_table_skip = function
  | "dref" -> Some(8)
  | "stsd" -> Some(8)
  | _ -> None

let rec dump_atom indent ({ tp; sz; data; _ } : QTFF.Atoms.t) =
  let ni = "     " ^ indent in
  let r = dump_atom ni in
  Printf.printf "%s%s -- %d bytes\n" indent tp sz;
  match data with
  | Leaf -> ()
  | Node l -> List.iter r l
  
let dump file =
  QTFF.Atoms.from_file file
    |> List.iter (dump_atom "")

let () =
  match Sys.argv with
  | [| _; file |] -> dump file
  | _ -> print_endline "Missing file name"

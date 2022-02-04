let input_binary_int_opt ic =
  try Some (input_binary_int ic)
  with End_of_file -> None

let dump_next_chunk ic = fun _ ->
  let dump sz =
    let tp = really_input_string ic 4 in
    let cur = pos_in ic in
    seek_in ic (cur + sz - 8);
    Printf.printf "%s -- %d bytes\n" tp sz
  in
  let parse_size = function
    | 0 -> dump ((in_channel_length ic) - (pos_in ic))
    | 1 -> failwith "TODO"
    | sz -> dump sz
  in
  ic |> input_binary_int_opt |> Option.map parse_size

let dump file =
  let ic = open_in_bin file in
  try
    ic |> dump_next_chunk |> Stream.from |> Stream.iter (fun _ -> ());
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e

let () =
  match Sys.argv with
  | [| _; file |] -> dump file
  | _ -> print_endline "Missing file name"

let input_binary_int_opt limit ic =
  if (pos_in ic) >= limit
  then None
  else Some (input_binary_int ic)

let dump_next_chunk_like indent limit ic = fun _ ->
  let dump sz =
    let tp = really_input_string ic 4 in
    let cur = pos_in ic in
    let end_of_chunk = cur + sz - 8 in
    Printf.printf "%s%s -- %d bytes\n" indent tp sz;
    seek_in ic end_of_chunk
  in
  ic |> input_binary_int_opt limit |> Option.map dump

let dump_chunk_like_table indent limit ic =
  ic |> dump_next_chunk_like indent limit |> Stream.from |> Stream.iter (fun _ -> ())

let is_rec_chunk = function
  | "clip" -> true
  | "dinf" -> true
  | "edts" -> true
  | "imap" -> true
  | "  in" -> true
  | "matt" -> true
  | "mdia" -> true
  | "minf" -> true
  | "moov" -> true
  | "rmda" -> true
  | "rmra" -> true
  | "stbl" -> true
  | "trak" -> true
  | "tref" -> true
  | _ -> false

let chunk_like_table_skip = function
  | "dref" -> Some(8)
  | "stsd" -> Some(8)
  | _ -> None

type entry = { tp : string; sz : int; indent : string; end_pos : int }

let seq_of_chunks indent limit ic =
  let parse_entry sz : entry =
    let tp = really_input_string ic 4 in
    let cur = pos_in ic in
    let end_pos = cur + sz - 8 in
    { tp; sz; indent; end_pos }
  in
  let parse_size = function
    | 0 -> parse_entry (limit - (pos_in ic))
    | 1 -> failwith "TODO"
    | sz -> parse_entry sz
  in
  let next = fun _ -> ic |> input_binary_int_opt limit |> Option.map parse_size
  in
  Stream.from next

let dump_thing ic { tp; sz; indent; end_pos } =
  Printf.printf "%s%s -- %d bytes\n" indent tp sz;
  seek_in ic end_pos

let rec dump_atom ic { tp; sz; indent; end_pos } =
  Printf.printf "%s%s -- %d bytes\n" indent tp sz;
  let i = "     " ^ indent in begin
    match tp |> chunk_like_table_skip with
    | Some(skip) ->
        seek_in ic (skip + pos_in ic);
        seq_of_chunks i end_pos ic |> Stream.iter (dump_thing ic)
    | None ->
        if is_rec_chunk tp then seq_of_chunks i end_pos ic |> Stream.iter (dump_atom ic)
  end;
  seek_in ic end_pos

let dump file =
  let ic = open_in_bin file in
  try
    seq_of_chunks "" (in_channel_length ic) ic
    |> Stream.iter (dump_atom ic);

    close_in ic
  with e ->
    close_in_noerr ic;
    raise e

let () =
  match Sys.argv with
  | [| _; file |] -> dump file
  | _ -> print_endline "Missing file name"

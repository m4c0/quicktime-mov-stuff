let repl run =
  let last_cmd = ref "" in
  let safe_run (str : string) =
    try 
      run str;
      last_cmd := str
    with
    | Failure f
    | Invalid_argument f
    | Sys_error f -> print_endline f
    | x -> print_endline ("Unhandled error: " ^ (Printexc.to_string x))
  in
  let read_line_opt () =
    try Some (read_line ())
    with End_of_file -> None 
  in
  let rec loop () =
    match read_line_opt () with
    | None -> ()
    | Some "" -> (safe_run !last_cmd; loop())
    | Some line -> (safe_run line; loop())
  in
  loop ();

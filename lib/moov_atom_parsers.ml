let elst (b : bytes) =
  Printf.printf
    "Version: %d - Flags: %d %d %d\n"
    (Bytes.get_int8 b 0)
    (Bytes.get_int8 b 1)
    (Bytes.get_int8 b 2)
    (Bytes.get_int8 b 3)
    ;

  let num_entries = Bytes.get_int32_be b 4 |> Int32.to_int in
  Printf.printf "Entries: %d\n" num_entries;

  for _ = 1 to num_entries do
    let d = Bytes.get_int32_be b 8 |> Int32.to_int in
    let i = Bytes.get_int32_be b 12 |> Int32.to_int in
    let r = Bytes.get_int32_be b 16 |> Int32.to_int in
    let fr = (float_of_int r) /. 65536.0 in
    Printf.printf "  Duration: %d - Time: %d - Rate: %f\n" d i fr
  done

let hexdump (b : bytes) =
  for n = 0 to (Bytes.length b) do
    let idx = n mod 16 in
    Printf.printf "%02x" (Bytes.get_int8 b n);
    print_string (match idx with 7 -> "    " | 15 -> "\n" | _ -> " ");
  done;
  print_newline ()
  
let parser_of = function
  | "elst" -> elst
  | _ -> hexdump

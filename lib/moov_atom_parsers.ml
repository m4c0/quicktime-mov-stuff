let get_i16 b idx = Bytes.get_int16_be b idx
let get_i32 b idx = Bytes.get_int32_be b idx |> Int32.to_int

let print_vf (b : bytes) =
  Printf.printf
    "Version: %d - Flags: %d %d %d\n"
    (Bytes.get_int8 b 0)
    (Bytes.get_int8 b 1)
    (Bytes.get_int8 b 2)
    (Bytes.get_int8 b 3)

let hexdump (b : bytes) =
  for n = 0 to (Bytes.length b) do
    let idx = n mod 16 in
    Printf.printf "%02x" (Bytes.get_int8 b n);
    print_string (match idx with 7 -> "    " | 15 -> "\n" | _ -> " ");
  done;
  print_newline ()

let hexdump_sub (b : bytes) (i : int) =
  let len = (Bytes.length b) - i in
  let sub = Bytes.sub b i len in
  hexdump sub
  
let elst (b : bytes) =
  print_vf b;

  let num_entries = Bytes.get_int32_be b 4 |> Int32.to_int in
  Printf.printf "Entries: %d\n" num_entries;

  for _ = 1 to num_entries do
    let d = get_i32 b 8 in
    let i = get_i32 b 12 in
    let r = get_i32 b 16 in
    let fr = (float_of_int r) /. 65536.0 in
    Printf.printf "  Duration: %d - Time: %d - Rate: %f\n" d i fr
  done

let mvhd (b : bytes) =
  print_vf b;

  Printf.printf
    "Creation time: %d - Modification time: %d\n"
    (get_i32 b 4)
    (get_i32 b 8);

  Printf.printf
    "Time scale = %d - Duration = %d\n"
    (get_i32 b 12)
    (get_i32 b 16);

  hexdump_sub b 20

let tkhd (b : bytes) =
  print_vf b;

  Printf.printf
    "Creation time: %d - Modification time: %d\n"
    (get_i32 b 4)
    (get_i32 b 8);

  Printf.printf
    "Track id = %d - Reserved = %d - Duration = %d - Reserved = %d.%d\n"
    (get_i32 b 12)
    (get_i32 b 16)
    (get_i32 b 20)
    (get_i32 b 24)
    (get_i32 b 28);

  Printf.printf
    "Layer = %d - Alternate Group = %d - Volume %d\n"
    (get_i16 b 32)
    (get_i16 b 34)
    (get_i16 b 36);

  hexdump_sub b 24

let parser_of = function
  | "elst" -> elst
  | "mdhd" -> mvhd (* almost equals to mvhd - anything after byte 20 isn't *)
  | "mvhd" -> mvhd
  | "tkhd" -> tkhd
  | _ -> hexdump

let get_i16 b idx = Bytes.get_uint16_be b idx
let get_i32 b idx = Bytes.get_int32_be b idx |> Int32.to_int

let print_vf (b : bytes) =
  Printf.printf
    "Version: %d - Flags: %d %d %d\n"
    (Bytes.get_uint8 b 0)
    (Bytes.get_uint8 b 1)
    (Bytes.get_uint8 b 2)
    (Bytes.get_uint8 b 3)

let hexdump (bs : bytes) =
  let bslen = Int.min (Bytes.length bs) 1024 in
  let sep idx acc chr = 
    let nacc = acc ^ (String.make 1 chr) in
    match idx mod 16 with
    | 7 -> (nacc, "    ")
    | 15 -> ("", "    [" ^ nacc ^ "]\n")
    | _ -> (nacc, " ")
  in
  let rec pad idx acc =
    print_string "  ";
    match sep idx acc ' ' with
    | ("", s) -> print_string s
    | (a, s) -> print_string s; pad (idx + 1) a
  in
  let rec prn idx acc =
    let b = Bytes.get_uint8 bs idx in
    let chr = if b >= 32 && b <= 127 then Char.chr b else ' ' in
    let (nacc, s) = sep idx acc chr in
    let nidx = idx + 1 in
    Printf.printf "%02x%s" b s;
    if nidx < bslen
    then prn nidx nacc
    else if bslen mod 16 > 0 then pad nidx nacc
  in
  prn 0 ""

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

  hexdump_sub b 36

let parser_of = function
  | "elst" -> elst
  | "mdhd" -> mvhd (* almost equals to mvhd - anything after byte 20 isn't *)
  | "mvhd" -> mvhd
  | "tkhd" -> tkhd
  | _ -> hexdump

let elst (ic : Subchannel.t) =
  let sz = Subchannel.input_binary_int ic in
  let tp = Subchannel.input_fourcc ic in
  Printf.printf "Size: %d - Type: [%s]" sz tp;
  print_string " - Version: ";
  Subchannel.input_byte ic |> print_int;
  print_string " - Flags: ";
  Subchannel.input_byte ic |> print_int;
  Subchannel.input_byte ic |> print_int;
  Subchannel.input_byte ic |> print_int;
  print_newline ();

  let num_entries = Subchannel.input_binary_int ic in
  Printf.printf "Entries: %d\n" num_entries;

  for _ = 1 to num_entries do
    let d = Subchannel.input_binary_int ic in
    let i = Subchannel.input_binary_int ic in
    let r = Subchannel.input_binary_int ic in
    let fr = (float_of_int r) /. 65536.0 in
    Printf.printf "  Duration: %d - Time: %d - Rate: %f\n" d i fr
  done
  
let parser_of = function
  | "elst" -> elst
  | x -> fun _ -> failwith (x ^ ": no support for changing it")

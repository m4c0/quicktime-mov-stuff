type target = Machine | Human
let print_single (tgt : target) ({ tp; offs; sz; children } : Atoms.t) =
  match tgt with
  | Machine -> Printf.printf "%s;%d;%d;%d\n" tp offs sz (List.length children)
  | Human ->
      if Atoms.is_recursive tp
      then Printf.printf "%s @%d size:%d (%d children)\n" tp offs sz (List.length children)
      else Printf.printf "%s @%d size:%d\n" tp offs sz

let current_file : string ref = ref "a.mov"

let new_kid_on_the_block tp sz i l =
  let max_len res ({ offs; sz; _ } : Atoms.t) = max res (offs + sz) in
  let offs = List.fold_left max_len i l in
  let atom : Atoms.t = { tp; offs; sz; children = [] } in
  atom :: l

(* *)

let append fmt tp sz =
  Moov_state.map_tree (new_kid_on_the_block tp sz 0)
  |> List.hd
  |> print_single fmt

let append_children fmt tp sz =
  let nkotb = new_kid_on_the_block tp sz in
  let mapper (a : Atoms.t) = { a with children = nkotb (a.offs + 8) a.children } in
  let _ = Moov_state.map_atom_at_cursor mapper in
  let ({ children; _ } : Atoms.t) = Moov_state.atom_at_cursor () in
  children |> List.hd |> print_single fmt

let dump () =
  let a = Moov_state.atom_at_cursor() in
  let rec dump_bytes n ic =
    let idx = n mod 16 in
    if n < a.sz then begin
      Printf.printf "%02x" (Subchannel.input_byte ic);
      print_string (match idx with 7 -> "    " | 15 -> "\n" | _ -> " ");
      dump_bytes (n + 1) ic
    end
    else if idx < 15 then print_newline ()
  in
  let extract ic =
    Subchannel.seek_in ic a.offs;
    dump_bytes 0 ic
  in
  Subchannel.open_with extract !current_file

let edit file =
  let new_list = Moov_state.map_tree (fun _ -> Atoms.from_file file) in
  let len = List.length new_list in
  if len > 0 then Moov_state.move_cursor 0 |> ignore;
  current_file := file;
  Printf.printf "%d\n" len

let jump fmt offs = Moov_state.move_cursor offs |> print_single fmt

let print fmt = 
  Moov_state.atom_at_cursor () |> print_single fmt 

let print_children fmt =
  let a = Moov_state.atom_at_cursor () in
  List.iter (print_single fmt) a.children

let print_roots fmt = Moov_state.iter_tree (print_single fmt)

let replace_size fmt len =
  Moov_state.map_atom_at_cursor (fun a -> { a with sz = len });
  Moov_state.atom_at_cursor () |> print_single fmt 

let replace_type fmt fourcc =
  Moov_state.map_atom_at_cursor (fun a -> { a with tp = fourcc });
  Moov_state.atom_at_cursor () |> print_single fmt 

let sort () =
  let by_offs ({ offs = oa; _ } : Atoms.t) ({ offs = ob; _ } : Atoms.t) = compare oa ob; in
  let rec sort_kids (a : Atoms.t) = { a with children = rec_sort a.children }
  and rec_sort a : Atoms.t list = a |> List.sort by_offs |> List.map sort_kids in
  Moov_state.map_tree rec_sort |> ignore

let verify () =
  let rec checker pos ({ tp; offs; sz; children } : Atoms.t) =
    let aend = offs + sz in

    if pos > offs
    then failwith (Printf.sprintf "expecting offset %d for %s but got %d" pos tp offs);

    if Atoms.is_recursive tp
    then begin
      if children = [] then failwith (Printf.sprintf "expecting children at %d for %s" pos tp);
      let cend = List.fold_left checker 0 children in
      if cend <> aend
      then 
        failwith
          (Printf.sprintf "children end mismatch at %d for %s - found %d - expected %d" offs tp cend aend)
    end
    else if children <> [] then failwith (Printf.sprintf "unexpected children at %d for %s" pos tp);

    aend
  in
  let file_size = try (Unix.stat !current_file).st_size with _ -> 0 in
  let tree_size = Moov_state.fold_tree checker 0 in
  if file_size = tree_size
  then Printf.printf "file size is %d\n" file_size
  else if file_size < tree_size
  then Printf.printf "file size will grow - disk: %d - tree: %d\n" file_size tree_size
  else Printf.printf "atoms are not covering the whole file - disk: %d - tree %d\n" file_size tree_size

let write () =
  let oc = open_out_gen [Open_wronly; Open_binary] 0o666 !current_file in
  let rec w (a : Atoms.t) =
    seek_out oc a.offs;
    output_binary_int oc a.sz;
    output_string oc a.tp;
    List.iter w a.children;
    while (pos_out oc < (a.offs + a.sz)) do output_byte oc 0 done
  in
  try
    Moov_state.iter_tree w;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


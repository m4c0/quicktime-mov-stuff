let undo_q : Cutter_data.movie list ref = ref []
let redo_q : Cutter_data.movie list ref = ref []

let add_undo (m : Cutter_data.movie) =
  undo_q := m :: !undo_q;
  m

let pop_undo () =
  undo_q := List.tl !undo_q

let purge () =
  undo_q := [];
  redo_q := []

let undo (cur : Cutter_data.movie) =
  match !undo_q with
  | [] -> failwith "nothing to undo"
  | hd :: tl ->
      undo_q := tl;
      redo_q := cur :: !redo_q;
      hd

let redo (cur : Cutter_data.movie) =
  match !redo_q with
  | [] -> failwith "nothing to undo"
  | hd :: tl ->
      redo_q := tl;
      undo_q := cur :: !undo_q;
      hd


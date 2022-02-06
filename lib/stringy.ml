let break_str_at str idx =
  let left = String.sub str 0 idx in
  let right = String.sub str (idx + 1) (String.length str - idx - 1) in
  (left, right)

let break_first_word str =
  String.index_opt str ' '
  |> Option.map (break_str_at str)
  |> Option.value ~default:(str, "")

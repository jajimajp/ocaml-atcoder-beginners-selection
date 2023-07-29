let rec insert l x = match l with
| [] -> [x]
| h :: t -> if h = x then l else h :: insert t x

let rec read_ints n =
  if n > 0 then
    let x = read_int () in
    x :: read_ints (n - 1)
  else
    []

let _ =
  let n = read_int () in
  let d = read_ints n in
  List.fold_left insert [] d
  |> List.length
  |> string_of_int
  |> print_endline


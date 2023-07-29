let n = read_int ()
let a =
  read_line ()
  |> Str.split (Str.regexp " ")
  |> List.map int_of_string

let rec divisible_count n =
  if n mod 2 = 0
    then 1 + divisible_count (n / 2)
    else 0

let _ =
  let ans = List.fold_left
    (fun p n -> let cnt = divisible_count n in if p < cnt then p else cnt)
    35
    a in
  ans |> string_of_int |> print_endline

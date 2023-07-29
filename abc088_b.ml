let n = read_int ()
let a = read_line () |> Str.split (Str.regexp " ") |> List.map int_of_string

let rec solve l a b = match l with
| [] -> (a - b)
| h :: [] -> (h + a - b)
| h1 :: h2 :: tl -> solve tl (h1 + a) (h2 + b)

let _ =
  let a = List.sort (fun a b -> Stdlib.compare b a) a in
  solve a 0 0 |> string_of_int |> print_endline

let rec digits n =
  if n = 0 then 0
  else (n mod 10) + digits (n / 10)

let solve n a b =
  let rec aux i ans =
    if i <= n then
      let d = digits i in
      if a <= d && d <= b then
        aux (i + 1) (ans + i)
      else
        aux (i + 1) ans
    else ans
  in aux 1 0

let _ =
  Scanf.scanf "%d %d %d\n" solve
  |> string_of_int
  |> print_endline

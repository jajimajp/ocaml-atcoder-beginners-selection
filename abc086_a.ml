let solve a b = if a * b mod 2 = 0 then "Even" else "Odd"

let _ =
  print_endline (
    Scanf.scanf "%d %d\n" solve
  )

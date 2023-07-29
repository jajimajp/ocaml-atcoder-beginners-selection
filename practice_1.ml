let solve a b c s =
  string_of_int (a + b + c) ^ " " ^ s

let _ =
  print_endline (Scanf.scanf "%d\n%d %d\n%s\n" solve)

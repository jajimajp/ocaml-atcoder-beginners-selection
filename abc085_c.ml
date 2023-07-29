let solve n y =
  let rec loop x y_ =
    let z = n - x - y_ in
    if 10000 * x + 5000 * y_ + 1000 * z = y then
      (x, y_, z)
    else
      if x + y_ = n then
        if x = n then (-1, -1, -1)
        else loop (x + 1) 0
      else loop x (y_ + 1)
  in loop 0 0

let _ =
  let (x, y, z) = Scanf.scanf "%d %d\n" solve in
  print_endline (string_of_int x ^ " " ^ string_of_int y ^ " " ^ string_of_int z)

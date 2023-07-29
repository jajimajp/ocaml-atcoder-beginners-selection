type record = int * int * int

let abs n = if n < 0 then -1 * n else n

let movable r1 r2 =
  let (t1, x1, y1) = r1 in
  let (t2, x2, y2) = r2 in
  let dist = abs (x1 - x2) + abs (y1 - y2) in
  dist <= t2 - t1
  && (t2 - t1 - dist) mod 2 = 0

let rec solve = function
| []
| _ :: [] -> true
| r1 :: r2 :: tl ->
    if movable r1 r2 then solve (r2 :: tl)
    else false

let _ =
  let n = read_int () in
  let rec loop i acc =
    if i >= n then acc
    else
      let r = Scanf.scanf "%d %d %d\n" (fun t x y -> (t, x, y)) in
      loop (i + 1) (r :: acc)
  in
  let rs = (0, 0, 0) :: List.rev (loop 0 []) in
  print_endline (
    if solve rs then "Yes" else "No"
  )
  

let rec solve = function
| [] -> true
| 'm'::'a'::'e'::'r'::'d'::t -> solve t
| 'r'::'e'::'m'::'a'::'e'::'r'::'d'::t -> solve t
| 'e'::'s'::'a'::'r'::'e'::t -> solve t
| 'r'::'e'::'s'::'a'::'r'::'e'::t -> solve t
| _ -> false

let accum s =
  let len = String.length s in
  let rec loop i acc =
    if i >= len then acc
    else loop (i + 1) (s.[i] :: acc)
  in loop 0 []

let _ =
  let s = read_line () in
  let cs = accum s in
  if solve cs then
    print_endline "YES"
  else
    print_endline "NO"


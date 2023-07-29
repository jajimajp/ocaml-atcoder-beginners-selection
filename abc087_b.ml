let ans = ref 0
let count () = ans := (!ans + 1)

let solve a b c x =
  for i = 0 to a do
    for j = 0 to b do
      for k = 0 to c do
        let total = 500 * i + 100 * j + 50 * k in
        if total = x then count ()
      done
    done
  done

let _ = Scanf.scanf "%d\n%d\n%d\n%d\n" solve
let _ = (!ans) |> string_of_int |> print_endline

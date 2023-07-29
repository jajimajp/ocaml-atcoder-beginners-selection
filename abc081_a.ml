let ctoi = function
  | '1' -> 1
  | _ -> 0

let ans =
  Scanf.scanf "%c%c%c\n" (fun s1 s2 s3 ->
    List.fold_left (+) 0 (
      List.map ctoi [s1; s2; s3]
    )
  )

  let _ = ans |> string_of_int |> print_endline

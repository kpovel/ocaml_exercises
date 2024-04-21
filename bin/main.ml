let rec duplicate list =
  match list with [] -> [] | h :: t -> h :: h :: duplicate t

let () =
  Printf.printf "\n";
  let result = duplicate [ "a"; "b"; "c"; "c"; "d" ] in
  Printf.printf "%s" (String.concat ", " result)

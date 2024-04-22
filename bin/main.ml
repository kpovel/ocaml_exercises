let split list position =
  let rec aux list position current_position =
    match list with
    | [] -> ([], [])
    | h :: t ->
        if position > current_position then
          let x, y = aux t position (current_position + 1) in
          (h :: x, y)
        else ([ h ], t)
  in
  aux list position 1

let () =
  Printf.printf "\n";
  let x, y = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3 in
  Printf.printf "%s" (String.concat ", " x);
  Printf.printf "\n";
  Printf.printf "%s" (String.concat ", " y)

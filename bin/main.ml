let rec remove_at position list =
  match list with
  | [] -> []
  | hd :: l -> if position = 0 then l else hd :: remove_at (position - 1) l

let () =
  Printf.printf "\n";
  let result = remove_at 1 [ "a"; "b"; "c"; "d" ] in
  List.iter (fun x -> Printf.printf "%s, " x) result;
  Printf.printf "\n"

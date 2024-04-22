let rec insert_at item n list =
  match list with
  | [] -> []
  | hd :: list ->
      if n = 0 then item :: hd :: list else hd :: insert_at item (n - 1) list

let () =
  Printf.printf "\n";
  let result = insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] in
  List.iter (fun x -> Printf.printf "%s, " x) result;
  Printf.printf "\n"

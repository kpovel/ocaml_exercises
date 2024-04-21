let encode list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _unused as t) ->
        (* Printf.printf "A: %s, B: %s\n" a b; *)
        Printf.printf "Unused list: ";
        List.iter (fun x -> Printf.printf "%s, " x) _unused;
        Printf.printf "\nUsed list:   ";
        List.iter (fun x -> Printf.printf "%s, " x) t;
        Printf.printf "\n";
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)

let () =
  Printf.printf "\n";
  let input =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in

  let result = encode input in

  List.iter
    (fun (count, string) ->
      Printf.printf "Count: %d, String: %s\n" count string)
    result

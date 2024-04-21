let encode l =
  let mapped = List.map (fun x -> (1, x)) l in
  List.rev
    (List.fold_left
       (fun acc (r, c) ->
         match acc with
         | [] -> [ (r, c) ]
         | (count, last_char) :: list ->
             if last_char = c then (count + 1, last_char) :: list
             else (r, c) :: (count, last_char) :: list)
       [] mapped)

let () =
  let result =
    encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  List.iter
    (fun (count, string) ->
      Printf.printf "Count: %d, String: %s\n" count string)
    result

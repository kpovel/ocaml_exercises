type 'a rle = One of 'a | Many of int * 'a

let encode list =
  let rec aux count acc list =
    match list with
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) -> (
        match a = b with
        | true -> aux (count + 1) acc t
        | false -> aux 0 ((count + 1, a) :: acc) t)
  in
  let list = List.rev (aux 0 [] list) in
  List.map
    (fun (count, str) -> if count = 1 then One str else Many (count, str))
    list

let () =
  Printf.printf "\n";
  let input =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in

  let result = encode input in

  List.iter
    (fun el ->
      match el with
      | One str -> Printf.printf "Got one element of %s\n" str
      | Many (count, str) -> Printf.printf "Got %n elements of %s\n" count str)
    result

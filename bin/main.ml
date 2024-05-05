let rec take n l =
  match n = 0 with
  | true -> []
  | false ->
    (match l with
     | [] -> []
     | hd :: tl -> hd :: take (n - 1) tl)
;;

let rec drop n l =
  match n = 0 with
  | true -> l
  | false ->
    (match l with
     | [] -> []
     | _ :: t -> drop (n - 1) t)
;;

let rec merge x y =
  match x, y with
  | [], l -> l
  | l, [] -> l
  | hx :: tx, hy :: ty ->
    (match hx < hy with
     | true -> hx :: merge tx (hy :: ty)
     | false -> hy :: merge (hx :: tx) ty)
;;

let rec msort l =
  match l with
  | [] -> []
  | [ x ] -> [ x ]
  | _ ->
    let half_len = List.length l / 2 in
    let left = take half_len l in
    let right = drop half_len l in
    merge (msort left) (msort right)
;;

let () =
  msort [ [ 'o'; 'n'; 'e' ]; [ 't'; 'w'; 'o' ]; [ 't'; 'h'; 'r'; 'e'; 'e' ] ]
  |> List.sort (fun x y -> Stdlib.compare x y)
  |> List.map (fun x -> String.concat ";" (List.map (String.make 1) x))
  |> List.iter (fun x -> Stdio.printf "[%s]," x)
in
Stdio.printf "\n"

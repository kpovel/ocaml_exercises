open Base

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

let rec list_of_elements n =
  match n = 0 with
  | true -> []
  | false -> n :: list_of_elements (n - 1)
;;

let () =
  let list_with_length = [ 1_000; 10_000; 100_000; 1_000_000 ] in
  List.iter
    ~f:(fun x ->
      let start = Unix.gettimeofday () in
      let _ = msort @@ list_of_elements x in
      let stop = Unix.gettimeofday () in
      Stdio.printf "Execution time of %i element: %fs\n%!" x (stop -. start))
    list_with_length
in
Stdio.printf "\n"

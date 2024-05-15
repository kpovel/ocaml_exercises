open Core

let rec map ~f l =
  match l with
  | [] -> []
  | hd :: tl -> f hd :: map ~f tl
;;

let rec member x l =
  match l with
  | [] -> false
  | hd :: tl -> hd = x || member x tl
;;

let member_all x ls = List.map ls ~f:(member x) |> List.exists ~f:not |> not
let mapll ~f l = map ~f:(map ~f:(map ~f)) l

let truncate len ll =
  let rec trunc len list =
    match list with
    | [] -> []
    | hd :: tl -> if len <= 1 then [ hd ] else hd :: trunc (len - 1) tl
  in
  map ~f:(trunc len) ll
;;

let rec firstelts n ll =
  let first n l =
    match l with
    | [] -> n
    | hd :: _ -> hd
  in
  map ~f:(first n) ll
;;

let () =
  firstelts 69 [ [ 4; 2; 0 ]; [ 10; 20; 30 ]; [] ]
  |> List.iter ~f:(fun x -> Printf.printf "%d, " x);
  ()
;;

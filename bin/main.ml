let rec map f l =
  match l with
  | [] -> []
  | hd :: tl -> f hd :: map f tl
;;

let rec mapl f l =
  match l with
  | [] -> []
  | hd :: tl -> map f hd :: mapl f tl
;;

let () =
  let _ = mapl (fun x -> x * 2) [ [ 4 ]; [ 2 ]; [ 0 ]; [ 6; 9 ] ] in
  Stdio.printf "\n"
;;

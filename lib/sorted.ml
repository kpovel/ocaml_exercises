let rec is_sorted l =
  match l with
  | [] -> true
  | [ _ ] -> true
  | x :: y :: tl -> if x < y then is_sorted @@ (y :: tl) else false
;;

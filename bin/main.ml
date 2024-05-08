open Base

let fst (x, _) = x
let snt (_, y) = y

let rec lookup x l =
  match l with
  | [] -> None
  | (k, v) :: tl ->
    (match k = x with
     | true -> Some v
     | false -> lookup x tl)
;;

let rec add k v l =
  match l with
  | [] -> [ k, v ]
  | (k', v') :: tl ->
    (match k' = k with
     | true -> (k, v) :: tl
     | false -> (k', v') :: add k v l)
;;

let rec remove k l =
  match l with
  | [] -> []
  | (k', v') :: tl ->
    (match k' = k with
     | true -> tl
     | false -> (k', v') :: remove k tl)
;;

let key_exists k l =
  match lookup k l with
  | Some _ -> true
  | None -> false
;;

let () =
  let census = [ 1, 4; 2, 2; 3, 2; 4, 3; 5, 1; 6, 2 ] in
  match lookup 4 census with
  | Some vl -> Stdlib.Printf.printf "We god em %d" vl
  | None -> Stdlib.Printf.printf "Noting found"
;;

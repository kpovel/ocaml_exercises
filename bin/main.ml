open Core

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

let n_kens l =
  let rec acc count l =
    match l with
    | [] -> count
    | _ :: tl -> acc (count + 1) tl
  in
  acc 0 l
;;

let rec replace k v l =
  match l with
  | [] -> raise_s (Sexp.Atom "Not_found")
  | (k', v') :: tl ->
    (match k' = k with
     | true -> (k, v) :: tl
     | false -> (k', v') :: replace k v tl)
;;

let rec mkdict keys values =
  match keys, values with
  | [], [] -> []
  | [], _ | _, [] -> raise_s (Sexp.Atom "Invalid_argument")
  | k :: ks, v :: vs -> (k, v) :: mkdict ks vs
;;

let rec mklists dict =
  match dict with
  | [] -> [], []
  | (k, v) :: hd ->
    (match mklists hd with
     | ks, vs -> k :: ks, v :: vs)
;;

let rec todict l =
  match l with
  | [] -> []
  | (k, v) :: tl ->
    let acc = todict tl in
    (match key_exists k acc with
     | true -> acc
     | false -> (k, v) :: acc)
;;

let rec union a b =
  match b with
  | [] -> a
  | (k, v) :: tl ->
    let result = union a tl in
    (match key_exists k a with
     | true -> result
     | false -> (k, v) :: result)
;;

let () =
  let census = union [ 1, 1; 2, 2; 3, 3 ] [ 4, 4; 1, 2 ] in
  List.iter census ~f:(fun (k, v) -> Stdlib.Printf.printf "Key: %d, Value: %d\n" k v);
  ()
;;

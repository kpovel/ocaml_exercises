type 'a tree =
  | Br of 'a * 'a tree * 'a tree
  | Lf

type 'a newtree = Br' of 'a * 'a newtree list

let rec sizen (Br' (_, tl)) =
  List.map (fun x -> sizen x) tl |> List.fold_left (fun acc x -> acc + x) 1
;;

let rec sumn (Br' (x, tl)) = List.map sumn tl |> List.fold_left ( + ) x
let rec mapn (Br' (x, tl)) f = Br' (f x, List.map (fun x -> mapn x f) tl)

let rec pp_tree pp_elem fmt = function
  | Lf -> Format.fprintf fmt "Lf"
  | Br (x, left, right) ->
    Format.fprintf
      fmt
      "Br (%a, %a, %a)"
      pp_elem
      x
      (pp_tree pp_elem)
      left
      (pp_tree pp_elem)
      right
;;

let pp_tuple fmt (i, s) = Format.fprintf fmt "(%d, %s)" i s

let rec size tr =
  match tr with
  | Lf -> 0
  | Br (_, l, r) -> 1 + size l + size r
;;

let rec total tr =
  match tr with
  | Lf -> 0
  | Br (x, l, r) -> x + total l + total r
;;

let rec max_depth tr =
  match tr with
  | Lf -> 0
  | Br (_, l, r) -> 1 + Int.max (max_depth l) (max_depth r)
;;

let rec list_of_tree tr =
  match tr with
  | Lf -> []
  | Br (x, l, r) -> list_of_tree l @ [ x ] @ list_of_tree r
;;

let rec lookup tr k =
  match tr with
  | Lf -> None
  | Br ((k', v), l, r) ->
    if k = k' then Some v else if k' < k then lookup l k else lookup r k
;;

let rec insert tr k v =
  match tr with
  | Lf -> Br ((k, v), Lf, Lf)
  | Br ((k', v'), l, r) ->
    if k = k'
    then Br ((k, v), l, r)
    else if k < k'
    then Br ((k', v'), insert l k v, r)
    else Br ((k', v'), l, insert r k v)
;;

let rec exists tr v =
  match tr with
  | Lf -> false
  | Br (v', l, r) -> v = v' || exists l v || exists r v
;;

let rec flip tr =
  match tr with
  | Lf -> Lf
  | Br (x, l, r) -> Br (x, flip r, flip l)
;;

let rec same_shape tr tr' =
  match tr, tr' with
  | Lf, Lf -> true
  | Br (_, l, r), Br (_, l', r') -> same_shape l l' && same_shape r r'
  | _ -> false
;;

let rec tree_of_list l =
  match l with
  | [] -> Lf
  | (k, v) :: tl -> insert (tree_of_list tl) k v
;;

let rec merge t t' = tree_of_list (list_of_tree t @ list_of_tree t')

let () =
  let tr = Br' (4, [ Br' (2, [ Br' (0, [ Br' (6, [ Br' (9, []) ]) ]) ]) ]) in
  sizen tr |> Format.printf "%d\n"
;;

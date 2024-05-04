let rec insert_sort x l =
  match l with
  | [] -> [ x ]
  | hd :: tl -> if x <= hd then x :: hd :: tl else hd :: insert_sort x tl
;;

let rec sort l =
  match l with
  | [] -> []
  | hd :: tl -> insert_sort hd @@ sort tl
;;

let () =
  let result = sort [ 6; 9; 4; 2; 0 ] in
  List.iter (fun x -> Printf.printf "%d" x) result;
  Printf.printf "\n"
;;

let rec concat l =
  match l with
  | [] -> []
  | hd :: tl -> hd @ concat tl
;;

let rec all_true l =
  match l with
  | [] -> true
  | hd :: tl -> if not (List.mem true hd) then false else all_true tl
;;

let count_exclamation s = String.split_on_char '!' s |> List.length |> ( + ) (-1)

let exclamation_to_period s =
  String.map
    (fun c ->
      match c with
      | '!' -> '.'
      | n -> n)
    s
;;

let str_concat l = String.concat "" l

let buf_concat l =
  let buf = Buffer.create 100 in
  List.iter (Buffer.add_string buf) l;
  Buffer.contents buf
;;

let count_ocamls s =
  let count = ref 0 in
  for i = 0 to String.length s - 5 do
    if String.sub s i 5 |> String.starts_with ~prefix:"OCaml" then count := !count + 1
  done;
  !count
;;

let () = count_ocamls "OCamlmycaml" |> Stdlib.Printf.printf "%d \n"

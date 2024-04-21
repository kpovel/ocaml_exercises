let rec without_last list =
  match list with
  | [] | [ _ ] -> []
  | first :: list -> first :: without_last list

let rec is_palindrome list =
  match list with
  | [] -> true
  | [ _ ] -> true
  | [ x; y ] -> x = y
  | first :: list -> (
      let last = List.nth list (List.length list - 1) in
      match first = last with
      | true ->
          let without_last = without_last list in
          is_palindrome without_last
      | false -> false)

let () =
  Printf.printf "\n";

  let list = [ "x"; "a"; "m"; "a"; "x" ] in
  let result = is_palindrome list in
  Printf.printf "%B\n" result;
  let result = is_palindrome [ "a"; "b" ] in
  Printf.printf "%B\n" result

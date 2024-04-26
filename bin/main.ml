let rec pow x n = if n = 1 then x else x * pow x (n - 1)

let () =
  let result = pow 5 3 in
  Printf.printf "%d" result;
  Printf.printf "\n"

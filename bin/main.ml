let rec range i n =
  match i <= n with true -> i :: range (i + 1) n | false -> []

let () =
  Printf.printf "\n";
  let result = range 4 9 in
  List.iter (fun x -> Printf.printf "%d, " x) result;
  Printf.printf "\n"

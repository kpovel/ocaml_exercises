let rec lotto_select n m =
  match n > 1 with
  | true -> Random.int m :: lotto_select (n - 1) m
  | false -> [ Random.int m ]

let () =
  Printf.printf "\n";
  let result = lotto_select 6 49 in
  List.iter (fun x -> Printf.printf "%d, " x) result;
  Printf.printf "\n"

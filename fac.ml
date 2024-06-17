let rec fac x =
  match x <= 1 with
  | true -> 1
  | false -> x * fac (x - 1)
;;

let () =
  for x = 0 to 10_000_000 do
    let _ = fac 100 in
    ()
  done
;;

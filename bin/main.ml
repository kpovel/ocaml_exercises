let round n =
  let f = floor n in
  match f +. 0.5 >= n with
  | true -> ceil n
  | false -> f
;;

let equidistan (x, y) (x', y') = (x +. x') /. 2., (y +. y') /. 2.

let parts n =
  let f = floor n in
  let rest = n -. f in
  f, rest
;;

let star n =
  assert (n >= 0. && n <= 1.);
  let position = n *. 50. |> round |> Float.to_int in
  let open Core in
  for x = 0 to 50 do
    match position = x with
    | true -> printf "*"
    | false -> printf " "
  done;
  printf "\n"
;;

let plot f range step =
  let pos = ref 0. in
  while range >= !pos do
    star (f !pos);
    pos := !pos +. step
  done
;;

open Core

let () = plot Float.sin Float.pi (Float.pi /. 20.)

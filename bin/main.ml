let isconsonant char =
  match char with 'a' | 'e' | 'i' | 'o' | 'u' -> true | _ -> false

let () =
  let result = isconsonant 'l' in
  Printf.printf "%b" result;
  Printf.printf "\n"

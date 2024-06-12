open Core

let print_histogram arr =
  Array.iteri arr ~f:(fun i x ->
    printf
      "For character '%c' (character number %d) the count is %d.()\n"
      (char_of_int i)
      i
      x)
;;

let channel_statistics ch =
  let file = In_channel.input_all ch in
  let lines = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let characters = ref 0 in
  let histogram = Array.init 256 ~f:(fun _ -> 0) in
  String.iter file ~f:(fun c ->
    characters := !characters + 1;
    let i = int_of_char c in
    histogram.(i) <- histogram.(i) + 1;
    match c with
    | '\n' ->
      lines := !lines + 1;
      words := !words + 1
    | '.' | '?' | '!' -> sentences := !sentences + 1
    | ' ' -> words := !words + 1
    | _ -> ());
  printf
    "There were %d lines, making up %d characters with %d words in %d sentences.\n"
    !lines
    !characters
    !words
    !sentences;
  print_histogram histogram
;;

let rec forloop f t exp =
  match f <= t with
  | true ->
    exp f;
    forloop (f + 1) t exp
  | false -> ()
;;

let sum arr =
  let sum = ref 0 in
  for i = 0 to Array.length arr - 1 do
    sum := !sum + arr.(i)
  done;
  !sum
;;

let to_lowercase char =
  let ascii = int_of_char char in
  match ascii with
  | x when x >= 65 && x <= 90 -> char_of_int (ascii + 32)
  | _ -> char
;;

let to_uppercase char =
  let ascii = int_of_char char in
  match ascii with
  | x when x >= 97 && x <= 122 -> char_of_int (ascii - 32)
  | _ -> char
;;

let () = to_lowercase 'Z' |> printf "%c\n"
(*let ch = In_channel.create "gregor.txt" in*)
(*channel_statistics ch;*)
(*In_channel.close ch*)

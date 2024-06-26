(* Text statistics *)
type stats = int * int * int * int * int array

(* Utility functions to retrieve parts of a stats value *)
let lines (l, _, _, _, _) = l
let characters (_, c, _, _, _) = c
let words (_, _, w, _, _) = w
let sentences (_, _, _, s, _) = s
let histogram (_, _, _, _, h) = h

let stats_from_channel in_channel =
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
  try
    while true do
      let line = input_line in_channel in
      lines := !lines + 1;
      characters := !characters + String.length line;
      String.iter
        (fun c ->
          let i = int_of_char c in
          histogram.(i) <- histogram.(i) + 1;
          match c with
          | '.' | '?' | '!' -> sentences := !sentences + 1
          | ' ' -> words := !words + 1
          | _ -> ())
        line
    done;
    0, 0, 0, 0, [||]
  with
  | End_of_file -> !lines, !characters, !words, !sentences, histogram
;;

let stats_from_file filename =
  let channel = open_in filename in
  let result = stats_from_channel channel in
  close_in channel;
  result
;;

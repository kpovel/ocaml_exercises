open Core

let rec read_dict () =
  Out_channel.(flush stdout);
  let i = In_channel.(input_line stdin) in
  match i with
  | Some i when not (String.equal i "") ->
    let i = Int.of_string i in
    let name = In_channel.(input_line_exn stdin) in
    (i, name) :: read_dict ()
  | _ -> []
;;

let entry_to_channel ch (k, v) =
  Out_channel.output_string ch (string_of_int k);
  Out_channel.output_char ch '\n';
  Out_channel.output_string ch v;
  Out_channel.output_char ch '\n'
;;

let dict_to_channel ch d = List.iter ~f:(entry_to_channel ch) d

let dict_to_file filename dict =
  let ch = Out_channel.create filename in
  dict_to_channel ch dict;
  Out_channel.close ch
;;

let entry_of_channel ch =
  let k = In_channel.input_line_exn ch |> Int.of_string in
  let v = In_channel.input_line_exn ch in
  k, v
;;

let rec dict_of_channel ch =
  try
    let e = entry_of_channel ch in
    e :: dict_of_channel ch
  with
  | End_of_file -> []
;;

let dict_of_file filename =
  let ch = In_channel.create filename in
  let dict = dict_of_channel ch in
  In_channel.close ch;
  dict
;;

let print_int_list l =
  List.map ~f:Int.to_string l |> String.concat ~sep:"; " |> printf "[ %s ]\n"
;;

let xtable filename =
  let x = In_channel.(input_line_exn stdin) |> Int.of_string in
  let rec range x =
    match x with
    | 0 -> []
    | x -> range (x - 1) @ [ x ]
  in
  let rangex = range x in
  let table =
    List.map ~f:(fun i -> List.map ~f:(( * ) i) rangex) rangex
    |> List.map ~f:(List.map ~f:Int.to_string)
    |> List.map ~f:(String.concat ~sep:"\t")
    |> String.concat ~sep:"\n"
    |> sprintf "%s\n"
  in
  let ch = Out_channel.create filename in
  Out_channel.output_string ch table;
  Out_channel.close ch
;;

let file_lines filename =
  let ch = In_channel.create filename in
  let file = In_channel.input_lines ch in
  In_channel.close ch;
  List.length file
;;

let copy_file filename1 filename2 =
  let file = In_channel.read_all filename1 in
  let ch = Out_channel.create filename2 in
  Out_channel.output_string ch file;
  Out_channel.close ch
;;

let () = copy_file "table.txt" "table2.txt"
(*file_lines "table.txt" |> printf "%d\n"*)
(*xtable "table.txt"*)
(*read_dict () |> dict_to_file "foo";*)
(*dict_of_file "foo" |> List.iter ~f:(fun (k, v) -> printf "(%d, %s)\n" k v)*)

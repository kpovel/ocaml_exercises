let read_file filename =
  let ch = In_channel.open_bin filename in
  let file = In_channel.input_all ch in
  close_in ch;
  file
;;

let () =
  match Sys.argv with
  | [| _; word; filename |] ->
    let file = read_file filename in
    let matched_lines =
      String.split_on_char '\n' file
      |> List.filter (fun l ->
        let contains = ref false in
        for i = 0 to String.length l - String.length word do
          let sub = String.sub l i (String.length word) in
          match sub = word with
          | true -> contains := true
          | false -> ()
        done;
        !contains)
    in
    List.iter (fun l -> Printf.printf "%s\n" l) matched_lines
  | _ -> Printf.printf "Usage: ./grep <word> <filename>\n"
;;

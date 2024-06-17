let read_file filename =
  let ch = In_channel.open_bin filename in
  let file = In_channel.input_all ch in
  close_in ch;
  String.trim file
;;

let write_file filename content =
  let ch = Out_channel.open_bin filename in
  output_string ch content;
  output_string ch "\n";
  close_out ch
;;

let () =
  match Sys.argv with
  | [| _; input; output |] ->
    let reversed_input =
      read_file input |> String.split_on_char '\n' |> List.rev |> String.concat "\n"
    in
    write_file output reversed_input
  | _ -> Printf.printf "Usage: ./reverse_lines <input> <output>\n"
;;

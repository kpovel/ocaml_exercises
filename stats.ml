(* Command line text file statistics program *)
try
  match Sys.argv with
  | [| _; filename |] ->
    let stats = Textstat.stats_from_file filename in
    Printf.printf "Words: %d\n" (Textstat.words stats);
    Printf.printf "Characters: %d\n" (Textstat.characters stats);
    Printf.printf "Sentences: %d\n" (Textstat.sentences stats);
    Printf.printf "Lines: %d\n" (Textstat.lines stats)
  | _ -> Printf.printf "Usage: stats <filename>\n"
with
| e ->
  Printf.printf "An error occurred: %s\n" (Printexc.to_string e);
  exit 1

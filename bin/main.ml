let () =
  let s = new Exercises.Classes.double_stack [ 4; 2; 0 ] in

  s#push 69;
  s#print;
  let _ = s#pop in
  s#print

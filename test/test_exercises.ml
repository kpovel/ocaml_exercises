open Alcotest
open Exercises

let suite =
  [ ( "is_sorted"
    , `Quick
    , fun _ ->
        check bool "not sorted list" false @@ Sorted.is_sorted [ 6; 9; 4; 2; 0 ];
        check bool "sorted list" true @@ Sorted.is_sorted [ 0; 2; 4; 6; 9 ];
        check bool "empty list" true @@ Sorted.is_sorted [] )
  ]
;;

let () = Alcotest.run "Dummy" [ "Sorted", suite ]

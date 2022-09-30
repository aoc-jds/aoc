
open Aoc
let codom_testable = Alcotest.testable pp_codom eq_codom

(* The tests *)
let test_one_empty () =
  Alcotest.(check codom_testable) "Empty evals to empty" [] (exec [])

let test_one_single () =
  Alcotest.(check codom_testable) "Singleton evals to empty" [] (exec [1])

let test_one_multiple () =
  Alcotest.(check codom_testable) 
    "2 or more produces string of diffs" 
    [INC;INC] (exec [1;2;3])

let test_one_example () =
  Alcotest.(check codom_testable)
    "Example from the website"
    [INC;INC;INC;DEC;INC;INC;INC;DEC;INC] 
    (exec [199;200;208;210;200;207;240;269;260;263])

(* Run it *)
let () =
  let open Alcotest in
  run "Adevnt of Code " [
    "one", [
      test_case "Empty list" `Quick test_one_empty;
      test_case "List with single item" `Quick test_one_single;
      test_case "List with multiple items" `Quick test_one_multiple;
      test_case "List from website" `Quick test_one_example;
    ];
    "two", [
    ];
  ]

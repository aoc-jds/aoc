
(* Run it *)
let () =
  let open Alcotest in
  run "Adevnt of Code " [
    "one", [
      test_case "Empty list" `Quick T_one.t_empty;
      test_case "List with single item" `Quick T_one.t_single;
      test_case "List with multiple items" `Quick T_one.t_multiple;
      test_case "List from website" `Quick T_one.t_example;
    ];
    "two", [
      test_case "Empty list" `Quick T_two.t_empty;
      test_case "Single entry list" `Quick T_two.t_single_entry;
      test_case "Site input list" `Quick T_two.t_total;
    ];
  ]
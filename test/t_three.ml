open Lib.Three

let pp_codom ppf = Fmt.pf ppf "%d"

let eq_codom x y = (x=y)

let codom_testable = Alcotest.testable pp_codom eq_codom

(** Test utiltities *)
let get_inputs =
  String.split_on_char '\n' Three_input.site_input 

(** The tests of the priority assignment function *)
let t_get_priority () = 
  Alcotest.(check codom_testable) "Lower case 'a' maps to 1" 1 (get_priority 'a');
  Alcotest.(check codom_testable) "Lower case 'z' maps to 1" 26 (get_priority 'z');
  Alcotest.(check codom_testable) "Lower case 'A' maps to 1" 27 (get_priority 'A');
  Alcotest.(check codom_testable) "Lower case 'Z' maps to 1" 52 (get_priority 'Z')

(* The tests *)
let t_empty () =
  Alcotest.(check codom_testable) "Empty evals to empty" 0 (exec [])

let t_sample () =
  Alcotest.(check codom_testable) "Sample list evals to expect value" 157 
    (exec [
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
      "PmmdzqPrVvPwwTWBwg";
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
      "ttgJtRGJQctTZtZT";
      "CrZsJsPPZsGzwwsLwLmpwMDw";
    ])

let t_total () =
  let inputs = get_inputs in
  Alcotest.(check codom_testable) "Calculate expected value for challenge" 0 
    (exec inputs)
  

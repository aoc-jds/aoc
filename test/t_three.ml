open Lib.Three.Part_one

(** Framework utilties for part one exec *)
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
  Alcotest.(check codom_testable) "Calculate expected value for challenge" 7821 
    (exec inputs)





open Lib.Three.Part_two

(** Framework utilties for part two exec *)
let pp_codom_partition = pp_group_t
let eq_codom_partition = equal_group_t
let partition_codom_testable = Alcotest.testable pp_codom_partition eq_codom_partition


let t_partition_empty () =
  Alcotest.(check partition_codom_testable) "Sample list evals to expect value" [] 
    (partition [])

let t_partition_one () =
  Alcotest.(check partition_codom_testable) "Sample list evals to expect value" [] 
    (partition [
      "vJrwpWtwJgWrhcsFMMfFFhFp";
    ])

let t_partition_two () =
  Alcotest.(check partition_codom_testable) "Sample list evals to expect value" [] 
    (partition [
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "vJrwpWtwJgWrhcsFMMfFFhFp";
    ])

let t_partition_three () =
  Alcotest.(check partition_codom_testable) "Sample list evals to expect value" [
      [
        "vJrwpWtwJgWrhcsFMMfFFhFp";
        "vJrwpWtwJgWrhcsFMMfFFhFp";
        "vJrwpWtwJgWrhcsFMMfFFhFp";
      ]
    ] 
    (partition [
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "vJrwpWtwJgWrhcsFMMfFFhFp";
    ])

let t_partition_four () =
  Alcotest.(check partition_codom_testable) "Sample list evals to expect value" [
      [
        "vJrwpWtwJgWrhcsFMMfFFhFp";
        "vJrwpWtwJgWrhcsFMMfFFhFp";
        "vJrwpWtwJgWrhcsFMMfFFhFp";
      ]
    ] 
    (partition [
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "vJrwpWtwJgWrhcsFMMfFFhFp";
    ])

let t_partition_six () =
  Alcotest.(check partition_codom_testable) "Sample list evals to expect value" [
      [
        "vJrwpWtwJgWrhcsFMMfFFhFp";
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
        "PmmdzqPrVvPwwTWBwg";
      ];
      [
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
        "ttgJtRGJQctTZtZT";
        "CrZsJsPPZsGzwwsLwLmpwMDw";
      ]
    ]
    (partition [
      "vJrwpWtwJgWrhcsFMMfFFhFp";
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
      "PmmdzqPrVvPwwTWBwg";
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
      "ttgJtRGJQctTZtZT";
      "CrZsJsPPZsGzwwsLwLmpwMDw";
    ])

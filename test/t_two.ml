open Lib.Two

(** Framework utiltities *)
let pp_codom ppf = function
  | (x:int) -> Fmt.pf ppf "%d" x

let eq_codom x y = (x=y)

let codom_testable = Alcotest.testable pp_codom eq_codom

(** Test utiltities *)
let get_inputs =
  let filelines = String.split_on_char '\n' Two_input.site_input in
  List.map ( fun line -> (line.[0], line.[2]) ) filelines

(** Part One - The tests *)
let t_empty () =
  Alcotest.(check codom_testable) "Empty evals to zero" 0 (Part_one.exec [])

let t_single_entry () =
    Alcotest.(check codom_testable) "Single evals to expected score" 
      8 (Part_one.exec [('A', 'Y')])

let t_total () =
  let inputs = get_inputs in 
  Alcotest.(check codom_testable) "Calculate site expected score" 
    10718 (Part_one.exec inputs)
      
(** Part Two - The tests *)
  
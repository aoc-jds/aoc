open Lib.One

let pp_t ppf = function
  | INC -> Fmt.pf ppf "INC"
  | DEC -> Fmt.pf ppf "DEC"
let eq_t a b = (a=b)

let rec pp_codom ppf = function
  | [] -> Fmt.pf ppf "[]"
  | [x] -> Fmt.pf ppf "%a" pp_t x
  | x :: xs -> Fmt.pf ppf "%a, %a" pp_t x pp_codom xs
  
let rec eq_codom x y =
  match x, y with
  | x :: xs, y :: ys ->
    eq_t x y && eq_codom xs ys
  | [], [] -> true
  | _ -> false

let codom_testable = Alcotest.testable pp_codom eq_codom

(* The tests *)
let t_empty () =
  Alcotest.(check codom_testable) "Empty evals to empty" [] (exec [])

let t_single () =
  Alcotest.(check codom_testable) "Singleton evals to empty" [] (exec [1])

let t_multiple () =
  Alcotest.(check codom_testable) 
    "2 or more produces string of diffs" 
    [INC;INC] (exec [1;2;3])

let t_example () =
  Alcotest.(check codom_testable)
    "Example from the website"
    [INC;INC;INC;DEC;INC;INC;INC;DEC;INC] 
    (exec [199;200;208;210;200;207;240;269;260;263])


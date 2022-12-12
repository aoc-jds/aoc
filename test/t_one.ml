open Lib.One

(* The tests *)
let t_empty () =
  Alcotest.(check int) "Empty evals to empty" 0 (exec "")
;;

let t_single_log () =
  Alcotest.(check int) "Sample evals to expected value" 24000 (exec "
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")
;;

let t_total () =
  Alcotest.(check int) "Site puzzle input" 64929 (exec One_input.site_input)
open Lib.One

(* The tests *)
let t_empty () =
  Alcotest.(check int) "Empty evals to empty" 0 (exec "")
;;

let t_single_log () =
  Alcotest.(check int) "Empty evals to empty" 0 (exec "
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
  
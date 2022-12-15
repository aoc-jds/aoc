open Lib.Four.Part_one

(* The tests *)
let t_empty () =
  Alcotest.(check int) "Empty evals to empty" 0 (exec [])

  
let t_sample () =
  Alcotest.(check int) "Empty evals to empty" 2 (exec [
    ((2, 4),(6, 8));
    ((2, 3),(4, 5));
    ((5, 7),(7, 9));
    ((2, 8),(3, 7));
    ((6, 6),(4, 6));
    ((2, 6),(4, 8))
  ])

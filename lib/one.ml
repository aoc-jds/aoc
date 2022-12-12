
type calories = int list
[@@deriving eq, show]

let rec mk_calorie_groups ?(acc = []) ?(group = []) entries : calories list =
  match entries with
  | "" :: entries' -> 
    mk_calorie_groups 
      ~acc:(group @ acc) 
      ~group:[] 
      entries'
  | e :: entries' -> 
    mk_calorie_groups 
      ~acc:acc
      ~group:((int_of_string e) :: group) 
      entries'
  | [] -> []
    
let exec (log : string) : int =
  let entries = String.split_on_char '\n' log in
  let _ = mk_calorie_groups entries in
  0

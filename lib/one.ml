
type calories = int list
[@@deriving eq, show]
type calorie_groups = calories list
[@@deriving show]

let rec mk_calorie_groups ?(acc = []) ?(group = []) entries : calories list =
  match entries with
  | "" :: entries' -> 
    mk_calorie_groups 
      ~acc:(group :: acc) 
      ~group:[] 
      entries'
  | e :: entries' -> 
    mk_calorie_groups 
      ~acc:acc
      ~group:((int_of_string e) :: group) 
      entries'
  | [] -> acc
    
let exec (log : string) : int =
  let entries = String.split_on_char '\n' log in
  let calorie_groups = mk_calorie_groups entries in
  let calorie_sums = List.map (List.fold_left (+) 0) calorie_groups in
  let max_calorie = List.fold_left max 0 calorie_sums in
  max_calorie

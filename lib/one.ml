
type calories = int list
[@@deriving eq, show]
type calorie_groups = calories list
[@@deriving show]

module Part_one = struct
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
end

module Part_two = struct
include Part_one

let exec (log : string) : int =
  let entries = String.split_on_char '\n' log in
  let calorie_groups = mk_calorie_groups entries in
  let calorie_sums = List.map (List.fold_left (+) 0) calorie_groups in
  print_endline (show_calories calorie_sums);
  let sorted_calorie_sums = List.sort compare calorie_sums in
  0
  (* let top_three_calorie_sum = 
    sorted_calorie_sums.[0] +
    sorted_calorie_sums.[1] +
    sorted_calorie_sums.[2] in
  top_three_calorie_sum *)
end
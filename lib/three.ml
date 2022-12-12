type sack_priorities_t = int list
[@@deriving show]

module Part_one = struct
  (** Internal constants for fast priority determination *)
  let lower_a_code = (Char.code 'a')
  let lower_z_code = (Char.code 'z')
  let upper_a_code = (Char.code 'A')
  let upper_z_code = (Char.code 'Z')
  let lower_a_offset = lower_a_code - 1
  let upper_a_offset = upper_a_code - 27

  (** Determine priority of a given commmon item *)
  let get_priority item =
    let item_code = Char.code item in 
    if item_code >= lower_a_code && item_code <= lower_z_code
    then 
      let ret = item_code - lower_a_offset in
      (* Printf.printf "'%c': priority = %d\n" item ret; *)
      ret
    else if item_code >= upper_a_code && item_code <= upper_z_code
    then
      let ret = item_code - upper_a_offset in
      (* Printf.printf "'%c': priority = %d\n" item ret; *)
      ret
    else 0

  (** Utility function *)
  let explode s = List.init (String.length s) (String.get s)

  (** Function to find common char between two strings and stop on first detection *)
  let rec project_common_item compartment_two compartment_one =
    match compartment_one with
    | i :: _ when (String.contains compartment_two i) -> i
    | i :: compartment_one' ->
      project_common_item compartment_two compartment_one'
    | [] -> '\x00'

  (** Determine priority of a given sack *)
  let get_sack_priority sack =
    let sack_length = (String.length sack) in
    let compartment_length = (sack_length / 2) in
    let compartment_one = String.sub sack 0 compartment_length in
    let compartment_two = String.sub sack compartment_length compartment_length in
    let common_item = project_common_item compartment_two (explode compartment_one) in
    get_priority common_item

  (** Determine priority for a list of sacks *)
  let exec (list_of_sacks : string list) : int = 
    let (sack_priorities : sack_priorities_t) = List.map get_sack_priority list_of_sacks in
    print_endline (show_sack_priorities_t sack_priorities);
    let total_priority = List.fold_left (+) 0 sack_priorities in
    total_priority
end 

module Part_two = struct
  include Part_one

  (** A group of elves is defined as group of 3 sacks *)
  type group_t = string list list
  [@@deriving eq, show]

  (** Partition a list of sacks into groups of 3 *)
  let rec partition ?(groups = []) individuals = 
    match individuals with
    | i1 :: i2 :: i3 :: is -> 
      [i1; i2; i3] :: (partition ~groups:groups is)
    | _ -> groups
end


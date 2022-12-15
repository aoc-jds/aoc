
module Part_one = struct
  type ranges_t = ((int * int) * (int * int))
  [@@deriving show]

  exception UnrecognizedInput
  let is_contained_within (ranges : ranges_t) =
    match ranges with 
    | ((a,b), (x, y)) -> 
      if (a <= x && b >= y) || (a >= x && b <= y)
      then begin
        print_endline (show_ranges_t ranges);
        1
      end
      else 0
  let exec (input : ranges_t list) : int =
    List.fold_left (fun acc ranges -> (is_contained_within ranges) + acc) 0 input
end
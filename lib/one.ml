type t = INC | DEC
type dom = int list
type codom = t list

let rec exec (readings : dom) : codom  = 
  match readings with
  | [] | [_] -> []
  | r :: r' :: rs ->
    let diff = if r > r' then DEC else INC in
    diff :: exec (r' :: rs)

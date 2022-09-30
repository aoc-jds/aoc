type t = INC | DEC
let pp_t ppf = function
  | INC -> Fmt.pf ppf "INC"
  | DEC -> Fmt.pf ppf "DEC"
let eq_t a b = (a=b)

type dom = int list
type codom = t list
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

let rec exec (readings : dom) : codom  = 
  match readings with
  | [] | [_] -> []
  | r :: r' :: rs ->
    let diff = if r > r' then DEC else INC in
    diff :: exec (r' :: rs)

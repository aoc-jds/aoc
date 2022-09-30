type t = INC | DEC
val pp_t : Format.formatter -> t -> unit
val pp_codom : Format.formatter -> t list -> unit
val eq_t : t -> t -> bool
val eq_codom : t list -> t list -> bool
val exec : int list -> t list
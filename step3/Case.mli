type t
type case  = ENTER | QUIT | NO
val new_case : bool -> bool -> bool -> bool -> t
  
val get_top : t -> bool
val get_bottom : t -> bool
val get_left : t -> bool
val get_right : t -> bool
val get_num : t -> int
val get_solve : t -> bool
val get_type_case : t -> case

val set_top : t -> bool -> t
val set_bottom : t -> bool -> t
val set_left : t -> bool -> t
val set_right : t -> bool -> t
val set_solve : t -> bool -> t
val set_type_case : t -> case -> t
val set_num : t -> int -> t

type t

val init_lab: int -> int -> unit
val solve_lab: (int * int) -> (int * int) -> bool
val aff_lab : Sdlvideo.surface -> unit

exception Lab_exception of string

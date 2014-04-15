type t

val init_lab: int -> int -> unit
val aff_lab : Sdlvideo.surface -> unit

exception Lab_exception of string

type type_door = UP | UP_LEFT | LEFT | BOTTOM_LEFT | BOTTOM | BOTTOM_RIGHT | RIGHT | UP_RIGHT
type type_case  = ENTER | QUIT | NO

exception Lab_exception of string

module type DOOR =
sig
  type t
  val new_doors_list : int -> int -> t list
  val open_door : t list -> bool -> type_door -> t list
  val get_type_door : t -> type_door
  val is_open : t -> bool
end

module DoorSquare : DOOR
module DoorHexa : DOOR

module type CASE =
sig
  type t
  val new_case : int -> int -> t
  val get_door_square : t -> DoorSquare.t list
  val get_door_hexa : t -> DoorHexa.t list
  val set_num : t -> int -> t
  val get_num : t -> int
  val set_solve : t -> bool -> t
  val get_solve : t -> bool
  val check_door : t array ref -> int -> int -> int -> int -> unit
  val set_type_case : t -> type_case -> t
  val get_type_case : t -> type_case
end

module CaseSquare : CASE
module CaseHexa : CASE

module type LAB =
  sig
    type t
    val init_lab_case : int -> int -> unit
    val check_lab_finish : unit -> bool
    val set_all_case_num : int -> int -> unit
    val make_lab : unit -> unit
    val getValCase : (int * int) -> t
    val aff_lab : Sdlvideo.surface -> unit
    val affCase : t -> int -> int -> Sdlvideo.surface -> unit
    val go_that_way_square : int -> int -> CaseSquare.t -> bool
    val go_that_way_hexa : int -> int -> CaseHexa.t -> bool
    val next_position_square : DoorSquare.t -> (int * int) -> (int * int)
    val next_position_hexa : DoorHexa.t -> (int * int) -> (int * int)
    val solve_lab: (int * int) -> (int * int) -> bool
  end

module SquareLab : LAB
module HexaLab : LAB


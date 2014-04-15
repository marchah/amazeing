type type_door = UP | UP_LEFT | LEFT | BOTTOM_LEFT | BOTTOM | BOTTOM_RIGHT | RIGHT | UP_RIGHT
type type_case  = ENTER | QUIT | NO

exception Lab_exception of string


(********************   DOOR   **********************************)



module type DOOR =
sig
  type t
  val new_doors_list : int -> int -> t list
  val open_door : t list -> bool -> type_door -> t list
  val get_type_door : t -> type_door
  val is_open : t -> bool
end

module DoorSquare : DOOR =
struct
  type t = {mutable is_open : bool; x : int; y : int; door : type_door}

  let new_doors_list pos_x pos_y =
    {is_open = false; x = pos_x; y = pos_y; door = UP}
    ::{is_open = false; x = pos_x; y = pos_y; door = LEFT}
    ::{is_open = false; x = pos_x; y = pos_y; door = BOTTOM}
    ::{is_open = false; x = pos_x; y = pos_y; door = RIGHT}
    ::[]

  let set_is_open {is_open = _; x = pos_x; y = pos_y; door = d} bool =
    {is_open = bool; x = pos_x; y = pos_y; door = d}

  let rec set_door door pos bool count new_door =
    if (count = List.length door) then
      List.rev new_door
    else if (count = pos) then
      set_door door pos bool (count+1) ((set_is_open (List.nth door pos) bool)::new_door)
    else
      set_door door pos bool (count+1) ((List.nth door count)::new_door)

  let open_door door bool =
    function
      | UP	-> set_door door 0 bool 0 []
      | LEFT	-> set_door door 1 bool 0 []
      | BOTTOM	-> set_door door 2 bool 0 []
      | RIGHT	-> set_door door 3 bool 0 []
      | _	-> raise (Lab_exception "Error: invalid door!")


  let get_type_door {is_open = _; x = _; y = _; door = d} = d
  let is_open {is_open = bool; x = _; y = _; door = _} = bool

end


module DoorHexa : DOOR =
struct
  type t = {mutable is_open : bool; x : int; y : int; door : type_door}

  let new_doors_list pos_x pos_y =
    {is_open = false; x = pos_x; y = pos_y; door = UP}
    ::{is_open = false; x = pos_x; y = pos_y; door = UP_LEFT}
    ::{is_open = false; x = pos_x; y = pos_y; door = LEFT}
    ::{is_open = false; x = pos_x; y = pos_y; door = BOTTOM_LEFT}
    ::{is_open = false; x = pos_x; y = pos_y; door = BOTTOM}
    ::{is_open = false; x = pos_x; y = pos_y; door = BOTTOM_RIGHT}
    ::{is_open = false; x = pos_x; y = pos_y; door = RIGHT}
    ::{is_open = false; x = pos_x; y = pos_y; door = UP_RIGHT}
    ::[]

  let set_is_open {is_open = _; x = pos_x; y = pos_y; door = d} bool =
    {is_open = bool; x = pos_x; y = pos_y; door = d}

  let rec set_door door pos bool count new_door =
    if (count = List.length door) then
      List.rev new_door
    else if (count = pos) then
      set_door door pos bool (count+1) ((set_is_open (List.nth door pos) bool)::new_door)
    else
      set_door door pos bool (count+1) ((List.nth door count)::new_door)

  let open_door door bool =
    function
      | UP		-> set_door door 0 bool 0 []
      | UP_LEFT		-> set_door door 1 bool 0 []
      | LEFT		-> set_door door 2 bool 0 []
      | BOTTOM_LEFT	-> set_door door 3 bool 0 []
      | BOTTOM		-> set_door door 4 bool 0 []
      | BOTTOM_RIGHT	-> set_door door 5 bool 0 []
      | RIGHT		-> set_door door 6 bool 0 []
      | UP_RIGHT	-> set_door door 7 bool 0 []

  let get_type_door {is_open = _; x = _; y = _; door = d} = d
  let is_open {is_open = bool; x = _; y = _; door = _} = bool

end



(********************   CASE   **********************************)



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


module CaseSquare : CASE =
struct
  let count = ref 0
  let x = ref 0
  let y = ref 0
  type t = {mutable door : DoorSquare.t list; mutable num : int; mutable solve : bool; mutable type_case : type_case}

  let new_case x_max y_max =
    begin
      count := !count + 1;
      begin
	if (!x + 1 == x_max) then
	  begin
	    x := 0;
	    y := !y + 1
	  end;
      end;
      x := !x + 1;
      y := !y + 1;
      {door = DoorSquare.new_doors_list (!x - 1) (!y - 1); num = !count; solve = false; type_case = NO}
    end

  let get_door_square {door = d; num = _; solve = _; type_case = _} = d
  let get_door_hexa {door = d; num = _; solve = _; type_case = _} = []

  let set_solve {door = d; num = n; solve = _; type_case = t} bool = {door = d; num = n; solve = bool; type_case = t}
  let get_solve {door = _; num = _; solve = s; type_case = _} = s


  let get_door {door = d; num = _; solve = _; type_case = _} = d
  let set_num {door = d; num = _; solve = s; type_case = t} n = {door = d; num = n; solve = s; type_case = t}
  let get_num {door = _; num = n; solve = _; type_case = _} = n


  let set_door {door = d; num = n; solve = s; type_case = t} bool eDoor =
    {door = (DoorSquare.open_door d bool eDoor); num = n; solve = s; type_case = t}

  let set_type_case {door = d; num = n; solve = s; type_case = _} c = {door = d; num = n; solve = s; type_case = c}
  let get_type_case {door = _; num = _; solve = _; type_case = c} = c

  let set_all_case_num tcase num_src num_dest y_max x_max =
    let rec _set_all_case_num =
    function
      | pos when (pos >= (y_max * x_max))		->
        ()
      | pos when (get_num !tcase.(pos) == num_src)	->
        begin
          Array.set !tcase pos (set_num !tcase.(pos) num_dest);
          _set_all_case_num (pos+1)
        end
      | pos						->
        _set_all_case_num (pos+1)
    in _set_all_case_num 0


  let check_door tcase pos_y pos_x y_max x_max =
    if ((pos_x + 1 < x_max) && (get_num !tcase.(pos_x + pos_y * x_max + 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true RIGHT);
	Array.set !tcase (pos_x + pos_y * x_max + 1) (set_door !tcase.(pos_x + pos_y * x_max + 1) true LEFT);
        set_all_case_num tcase (get_num !tcase.(pos_x + pos_y * x_max + 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_x > 0) && (get_num !tcase.(pos_x + pos_y * x_max - 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true LEFT);
        Array.set !tcase (pos_x + pos_y * x_max - 1) (set_door !tcase.(pos_x + pos_y * x_max - 1) true RIGHT);
        set_all_case_num tcase (get_num !tcase.(pos_x + pos_y * x_max - 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_y > 0) && (get_num !tcase.(pos_x + (pos_y - 1) * x_max)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true UP);
        Array.set !tcase (pos_x + (pos_y - 1) * x_max) (set_door !tcase.(pos_x + (pos_y - 1) * x_max) true BOTTOM);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y - 1) * x_max)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_y + 1 < y_max) && (get_num !tcase.(pos_x + (pos_y + 1) * x_max)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true BOTTOM);
        Array.set !tcase (pos_x + (pos_y + 1) * x_max) (set_door !tcase.(pos_x + (pos_y + 1) * x_max) true UP);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y + 1) * x_max)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
        end
    else
      ()
end


module CaseHexa : CASE =
struct
  let count = ref 0
  let x = ref 0
  let y = ref 0
  type t = {mutable door : DoorHexa.t list; mutable num : int; mutable solve : bool; mutable type_case : type_case}

  let new_case x_max y_max =
    begin
      count := !count + 1;
      begin
	if (!x + 1 == x_max) then
	  begin
	    x := 0;
	    y := !y + 1
	  end;
      end;
      x := !x + 1;
      y := !y + 1;
      {door = DoorHexa.new_doors_list (!x - 1) (!y - 1); num = !count; solve = false; type_case = NO}
    end

  let set_solve {door = d; num = n; solve = _; type_case = t} bool = {door = d; num = n; solve = bool; type_case = t}
  let get_solve {door = _; num = _; solve = s; type_case = _} = s


  let get_door_square {door = d; num = _; solve = _; type_case = _} = []
  let get_door_hexa {door = d; num = _; solve = _; type_case = _} = d

  let set_num {door = d; num = _; solve = s; type_case = t} n = {door = d; num = n; solve = s; type_case = t}
  let get_num {door = _; num = n; solve = _; type_case = _} = n


  let set_door {door = d; num = n; solve = s; type_case = t} bool eDoor =
    {door = (DoorHexa.open_door d bool eDoor); num = n; solve = s; type_case = t}

  let set_type_case {door = d; num = n; solve = s; type_case = _} c = {door = d; num = n; solve = s; type_case = c}
  let get_type_case {door = _; num = _; solve = _; type_case = c} = c

  let set_all_case_num tcase num_src num_dest y_max x_max =
    let rec _set_all_case_num =
    function
      | pos when (pos >= (y_max * x_max))		->
        ()
      | pos when (get_num !tcase.(pos) == num_src)	->
        begin
          Array.set !tcase pos (set_num !tcase.(pos) num_dest);
          _set_all_case_num (pos+1)
        end
      | pos						->
        _set_all_case_num (pos+1)
    in _set_all_case_num 0


  let check_door tcase pos_y pos_x y_max x_max =
    if ((pos_x > 0) && (pos_y + 1 < y_max)
	     && (get_num !tcase.(pos_x + (pos_y + 1) * x_max - 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true BOTTOM_LEFT);
	Array.set !tcase (pos_x + (pos_y + 1) * x_max - 1) (set_door !tcase.(pos_x + (pos_y + 1) * x_max - 1) true UP_RIGHT);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y + 1) * x_max - 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_x > 0) && (pos_y > 0)
	     && (get_num !tcase.(pos_x + (pos_y - 1) * x_max - 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true UP_LEFT);
	Array.set !tcase (pos_x + (pos_y - 1) * x_max - 1) (set_door !tcase.(pos_x + (pos_y - 1) * x_max - 1) true BOTTOM_RIGHT);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y - 1) * x_max - 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_x + 1 < x_max) && (get_num !tcase.(pos_x + pos_y * x_max + 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true RIGHT);
	Array.set !tcase (pos_x + pos_y * x_max + 1) (set_door !tcase.(pos_x + pos_y * x_max + 1) true LEFT);
        set_all_case_num tcase (get_num !tcase.(pos_x + pos_y * x_max + 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_x > 0) && (get_num !tcase.(pos_x + pos_y * x_max - 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true LEFT);
        Array.set !tcase (pos_x + pos_y * x_max - 1) (set_door !tcase.(pos_x + pos_y * x_max - 1) true RIGHT);
        set_all_case_num tcase (get_num !tcase.(pos_x + pos_y * x_max - 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_x + 1 < x_max) && (pos_y + 1 < y_max)
	     && (get_num !tcase.(pos_x + (pos_y + 1) * x_max + 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true BOTTOM_RIGHT);
	Array.set !tcase (pos_x + (pos_y + 1) * x_max + 1) (set_door !tcase.(pos_x + (pos_y + 1) * x_max + 1) true UP_LEFT);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y + 1) * x_max + 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_y > 0) && (get_num !tcase.(pos_x + (pos_y - 1) * x_max)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true UP);
        Array.set !tcase (pos_x + (pos_y - 1) * x_max) (set_door !tcase.(pos_x + (pos_y - 1) * x_max) true BOTTOM);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y - 1) * x_max)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_y + 1 < y_max) && (get_num !tcase.(pos_x + (pos_y + 1) * x_max)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true BOTTOM);
        Array.set !tcase (pos_x + (pos_y + 1) * x_max) (set_door !tcase.(pos_x + (pos_y + 1) * x_max) true UP);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y + 1) * x_max)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else if ((pos_x + 1 < x_max) && (pos_y > 0)
	     && (get_num !tcase.(pos_x + (pos_y - 1) * x_max + 1)) != (get_num !tcase.(pos_x + pos_y * x_max))) then
      begin
	Array.set !tcase (pos_x + pos_y * x_max) (set_door !tcase.(pos_x + pos_y * x_max) true UP_RIGHT);
	Array.set !tcase (pos_x + (pos_y - 1) * x_max + 1) (set_door !tcase.(pos_x + (pos_y - 1) * x_max + 1) true BOTTOM_LEFT);
        set_all_case_num tcase (get_num !tcase.(pos_x + (pos_y - 1) * x_max + 1)) (get_num !tcase.(pos_x + pos_y * x_max)) y_max x_max;
      end
    else
      ()
end



(********************   LAB   **********************************)



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
    val solve_lab : (int * int) -> (int * int) -> bool
  end

(******************    Signature du functor   *********************************)

module type MAKELAB = functor (Case : CASE) -> LAB


module MakeLabSquare : MAKELAB = functor (Case : CASE) ->
  struct
    type t = CaseSquare.t
    let x_max = ref 0
    let y_max = ref 0
    let tcase = ref [||]

    let getValCase (x, y) =
      if (y >= !y_max || x >= !x_max || (y * !x_max + x) >= (!x_max * !y_max)) then
	raise (Lab_exception "Error: getValCase: invalid offset, it's too big!")
      else if (y < 0 || x < 0) then
	raise (Lab_exception "Error: getValCase: invalid offset, it's negative!")
      else
	!tcase.(y * !x_max + x)

    let check_lab_finish () =
      let save = (CaseSquare.get_num !tcase.(0)) in
      let rec _check_lab_finish =
	function
	  | pos when (pos >= (!y_max * !x_max))				-> true
	  | pos when (save != (CaseSquare.get_num !tcase.(pos)))	-> false
	  | pos								-> _check_lab_finish (pos+1)
      in _check_lab_finish 0

    let set_all_case_num num_src num_dest =
      let rec _set_all_case_num =
	function
	  | pos when (pos >= (!y_max * !x_max))				->
            ()
	  | pos when (CaseSquare.get_num !tcase.(pos) == num_src)	->
            begin
              Array.set !tcase pos (CaseSquare.set_num !tcase.(pos) num_dest);
              _set_all_case_num (pos+1)
            end
	  | pos								->
            _set_all_case_num (pos+1)
      in _set_all_case_num 0
      

    let make_lab () =
      let rec _make_lab y x =
	if (check_lab_finish () = true) then
	  ()
	else
	  begin
	    CaseSquare.check_door tcase y x !y_max !x_max;
	    _make_lab (Random.int !y_max) (Random.int !x_max)
	  end
      in _make_lab (Random.int !y_max) (Random.int !x_max)

    let init_lab_case y x =
      if ((y * x) >= Sys.max_array_length) then
	raise (Lab_exception "Error: Array size is too big!")
      else
	begin
	  Random.self_init ();
	  y_max := y;
	  x_max := x;
	  tcase := Array.init (y * x) (fun _ -> CaseSquare.new_case x y);
	  make_lab ()
	end

    let ground = Sdlloader.load_image "../images/ground.png"
    let ground2 = Sdlloader.load_image "../images/ground2.png"
    let wallv = Sdlloader.load_image "../images/wallv.png"
    let wallh = Sdlloader.load_image "../images/wallh.png"
      
    let affCase case x y screen =
      let pos = Sdlvideo.rect (x * 32) (y * 32) 0 0
      and pos_top = Sdlvideo.rect (x * 32) (y * 32 - 8) 0 0
      and pos_left = Sdlvideo.rect (x * 32 - 8) (y * 32) 0 0
      and pos_bottom = Sdlvideo.rect (x * 32) ((y + 1) * 32 - 8) 0 0
      and pos_right = Sdlvideo.rect ((x + 1) * 32 - 8) (y * 32) 0 0
      in let rec _affdoor case x y screen ndoor =
	   begin
	     if ndoor < 4 then
	       begin
		   if DoorSquare.is_open (List.nth (CaseSquare.get_door_square case) ndoor) = false then
		       (match DoorSquare.get_type_door (List.nth (CaseSquare.get_door_square case) ndoor) with
			 | UP		-> Sdlvideo.blit_surface ~src:wallh ~dst_rect:pos_top ~dst:screen ()
			 | BOTTOM	-> Sdlvideo.blit_surface ~src:wallh ~dst_rect:pos_bottom ~dst:screen ()
			 | LEFT		-> Sdlvideo.blit_surface ~src:wallv ~dst_rect:pos_left ~dst:screen ()
			 | RIGHT	-> Sdlvideo.blit_surface ~src:wallv ~dst_rect:pos_right ~dst:screen ()
			 | _		-> ());
		 _affdoor case x y screen (ndoor + 1)
	       end
	   end
	 in
	 begin
	   begin
	     if CaseSquare.get_solve case then
	       Sdlvideo.blit_surface ~src:ground2 ~dst_rect:pos ~dst:screen ()
	     else 
	       Sdlvideo.blit_surface ~src:ground ~dst_rect:pos ~dst:screen ()
	   end;
	   _affdoor case x y screen 0
	 end


    let aff_lab screen =
      let rec _aff_lab_y y screen =
	if y < !y_max then
	  begin
            let rec _aff_lab_yx y x screen =
              begin
		if x < !x_max then
		  begin
                    affCase (getValCase (x,y)) x y screen;
                    _aff_lab_yx y (x+1) screen
		  end
		else
		  ()
              end
            in _aff_lab_yx y 0 screen;
            _aff_lab_y (y + 1) screen
	  end
	else
	  ()
      in _aff_lab_y 0 screen


    let go_that_way_square come_from nbr case =
      if (nbr < List.length (CaseSquare.get_door_square case)
	  && DoorSquare.is_open (List.nth (CaseSquare.get_door_square case) nbr)
	  && (come_from mod ((List.length (CaseSquare.get_door_square case)) / 2)
		!= nbr mod ((List.length (CaseSquare.get_door_square case)) / 2)
	     || come_from = nbr)) then
	true
      else
	false

    let go_that_way_hexa come_from nbr case = false

    let next_position_square door (x,y) = match DoorSquare.get_type_door door with
      | UP              -> (x, y - 1)
      | UP_LEFT         -> (x - 1, y - 1)
      | LEFT            -> (x - 1, y)
      | BOTTOM_LEFT     -> (x - 1, y + 1)
      | BOTTOM          -> (x, y + 1)
      | BOTTOM_RIGHT    -> (x + 1, y + 1)
      | RIGHT           -> (x + 1, y)
      | UP_RIGHT        -> (x + 1, y - 1)

    let next_position_hexa door (x,y) = (x, y)

    let solve_lab (x_begin, y_begin) (x_end, y_end) =
      let rec solve_lab_rec come_from nbr = function
	| ((x,y), case) when CaseSquare.get_type_case case = QUIT ->
	  (Array.set !tcase (x + y * !x_max) (CaseSquare.set_solve !tcase.(x + y * !x_max) true) ; true)
	| ((x,y), case)						  ->
	  if (go_that_way_square come_from nbr case = true) then
	    match solve_lab_rec nbr 0 ((next_position_square (List.nth (CaseSquare.get_door_square case) nbr) (x,y)),
				       (getValCase (next_position_square (List.nth (CaseSquare.get_door_square case) nbr) (x,y)))) with
	      | true  ->  Array.set !tcase (x + y * !x_max) (CaseSquare.set_solve !tcase.(x + y * !x_max) true) ; true
	      | false ->  solve_lab_rec come_from (nbr + 1) ((x,y), case)
	  else if (nbr < List.length (CaseSquare.get_door_square case)) then
	    solve_lab_rec come_from (nbr + 1) ((x,y), case)
	  else
	    false
      in
      if (x_begin < 0 ||
	    x_end < 0 ||
	    y_begin < 0 ||
	    y_end < 0 ||
	    x_begin >= !x_max ||
	    x_end >= !x_max ||
	    y_begin >= !y_max ||
	    y_end >= !y_max)
      then
	raise (Lab_exception "Error: the case beginning or the case ending is invalid.")
    else
	begin
	  Array.set !tcase (x_begin + y_begin * !x_max) (CaseSquare.set_type_case !tcase.((x_begin + y_begin * !x_max)) ENTER);
	  Array.set !tcase (x_end + y_end * !x_max) (CaseSquare.set_type_case !tcase.((x_end + y_end * !x_max)) QUIT);
	  solve_lab_rec (-1) 0 ((x_begin, y_begin), (getValCase (x_begin, y_begin)));
	end

  end




module MakeLabHexa : MAKELAB = functor (Case : CASE) ->
  struct
    type t = CaseHexa.t
    let x_max = ref 0
    let y_max = ref 0
    let tcase = ref [||]

    let getValCase (x, y) =
      if (y >= !y_max || x >= !x_max || (y * !x_max + x) >= (!x_max * !y_max)) then
	raise (Lab_exception "Error: getValCase: invalid offset, it's too big!")
      else if (y < 0 || x < 0) then
	raise (Lab_exception "Error: getValCase: invalid offset, it's negative!")
      else
	!tcase.(y * !x_max + x)

    let check_lab_finish () =
      let save = (CaseHexa.get_num !tcase.(0)) in
      let rec _check_lab_finish =
	function
	  | pos when (pos >= (!y_max * !x_max))				-> true
	  | pos when (save != (CaseHexa.get_num !tcase.(pos)))		-> false
	  | pos								-> _check_lab_finish (pos+1)
      in _check_lab_finish 0

    let set_all_case_num num_src num_dest =
      let rec _set_all_case_num =
	function
	  | pos when (pos >= (!y_max * !x_max))				->
            ()
	  | pos when (CaseHexa.get_num !tcase.(pos) == num_src)		->
            begin
              Array.set !tcase pos (CaseHexa.set_num !tcase.(pos) num_dest);
              _set_all_case_num (pos+1)
            end
	  | pos								->
            _set_all_case_num (pos+1)
      in _set_all_case_num 0
      

    let make_lab () =
      let rec _make_lab y x =
	if (check_lab_finish () = true) then
	  ()
	else
	  begin
	    CaseHexa.check_door tcase y x !y_max !x_max;
	    _make_lab (Random.int !y_max) (Random.int !x_max)
	  end
      in _make_lab (Random.int !y_max) (Random.int !x_max)

    let init_lab_case y x =
      if ((y * x) >= Sys.max_array_length) then
	raise (Lab_exception "Error: Array size is too big!")
      else
	begin
	  Random.self_init ();
	  y_max := y;
	  x_max := x;
	  tcase := Array.init (y * x) (fun _ -> CaseHexa.new_case x y);
	  make_lab ()
	end

    let ground8 = Sdlloader.load_image "../images/ground8.bmp"
    let ground82 = Sdlloader.load_image "../images/ground82.bmp"
    let walltop = Sdlloader.load_image "../images/walltop.png"
    let wallbottom = Sdlloader.load_image "../images/wallbottom.png"
    let wallleft = Sdlloader.load_image "../images/wallleft.png"
    let wallright = Sdlloader.load_image "../images/wallright.png"
    let walltopleft = Sdlloader.load_image "../images/walltopleft.png"
    let walltopright = Sdlloader.load_image "../images/walltopright.png"
    let wallbottomleft = Sdlloader.load_image "../images/wallbottomleft.png"
    let wallbottomright = Sdlloader.load_image "../images/wallbottomright.png"
      
    let affCase case x y screen =
      let pos = Sdlvideo.rect (x * 76) (y * 76) 0 0
      in let rec _affdoor case x y screen ndoor =
	   begin
	     if ndoor < 8 then
	       begin
		 if DoorHexa.is_open (List.nth (CaseHexa.get_door_hexa case) ndoor) = false then
		   (match DoorHexa.get_type_door (List.nth (CaseHexa.get_door_hexa case) ndoor) with
		     | UP	    -> Sdlvideo.blit_surface ~src:walltop ~dst_rect:pos ~dst:screen ()
		     | UP_LEFT	    -> Sdlvideo.blit_surface ~src:walltopleft ~dst_rect:pos ~dst:screen ()
		     | LEFT         -> Sdlvideo.blit_surface ~src:wallleft ~dst_rect:pos ~dst:screen ()
		     | BOTTOM_LEFT  -> Sdlvideo.blit_surface ~src:wallbottomleft ~dst_rect:pos ~dst:screen ()
		     | BOTTOM	    -> Sdlvideo.blit_surface ~src:wallbottom ~dst_rect:pos ~dst:screen ()
		     | BOTTOM_RIGHT -> Sdlvideo.blit_surface ~src:wallbottomright ~dst_rect:pos ~dst:screen ()
		     | RIGHT	    -> Sdlvideo.blit_surface ~src:wallright ~dst_rect:pos ~dst:screen ()
		     | UP_RIGHT	    -> Sdlvideo.blit_surface ~src:walltopright ~dst_rect:pos ~dst:screen ());
		 _affdoor case x y screen (ndoor + 1)
	       end
	   end
	 in
	 begin
	   begin
	     if CaseHexa.get_solve case then
	       Sdlvideo.blit_surface ~src:ground82 ~dst_rect:pos ~dst:screen ()
	     else 
	       Sdlvideo.blit_surface ~src:ground8 ~dst_rect:pos ~dst:screen ()
	   end;
	   _affdoor case x y screen 0
	 end


    let aff_lab screen =
      let rec _aff_lab_y y screen =
	if y < !y_max then
	  begin
            let rec _aff_lab_yx y x screen =
              begin
		if x < !x_max then
		  begin
                    affCase (getValCase (x,y)) x y screen;
                    _aff_lab_yx y (x+1) screen
		  end
		else
		  ()
              end
            in _aff_lab_yx y 0 screen;
            _aff_lab_y (y + 1) screen
	  end
	else
	  ()
      in _aff_lab_y 0 screen


    let go_that_way_square come_from nbr case = false

    let go_that_way_hexa come_from nbr case =
      if (nbr < List.length (CaseHexa.get_door_hexa case)
	  && DoorHexa.is_open (List.nth (CaseHexa.get_door_hexa case) nbr)
	  && (come_from mod ((List.length (CaseHexa.get_door_hexa case)) / 2)
		!= nbr mod ((List.length (CaseHexa.get_door_hexa case)) / 2)
		|| come_from = nbr)) then
	true
      else
	false

    let next_position_hexa door (x,y) = match DoorHexa.get_type_door door with
      | UP              -> (x, y - 1)
      | UP_LEFT         -> (x - 1, y - 1)
      | LEFT            -> (x - 1, y)
      | BOTTOM_LEFT     -> (x - 1, y + 1)
      | BOTTOM          -> (x, y + 1)
      | BOTTOM_RIGHT    -> (x + 1, y + 1)
      | RIGHT           -> (x + 1, y)
      | UP_RIGHT        -> (x + 1, y - 1)

    let next_position_square door (x,y) = (x, y)

    let solve_lab (x_begin, y_begin) (x_end, y_end) =
      let rec solve_lab_rec come_from nbr = function
	| ((x,y), case) when CaseHexa.get_type_case case = QUIT   ->
	  (Array.set !tcase (x + y * !x_max) (CaseHexa.set_solve !tcase.(x + y * !x_max) true) ; true)
	| ((x,y), case)                                           ->
	  if (go_that_way_hexa come_from nbr case = true) then
	    match solve_lab_rec nbr 0 ((next_position_hexa (List.nth (CaseHexa.get_door_hexa case) nbr) (x,y)),
				       (getValCase (next_position_hexa (List.nth (CaseHexa.get_door_hexa case) nbr) (x,y)))) with
	      | true  ->  Array.set !tcase (x + y * !x_max) (CaseHexa.set_solve !tcase.(x + y * !x_max) true) ; true
	      | false ->  solve_lab_rec come_from (nbr + 1) ((x,y), case)
	  else if (nbr < List.length (CaseHexa.get_door_hexa case)) then
	    solve_lab_rec come_from (nbr + 1) ((x,y), case)
	  else
	    false
      in
      if (x_begin < 0 ||
	    x_end < 0 ||
	    y_begin < 0 ||
	    y_end < 0 ||
	    x_begin >= !x_max ||
	    x_end >= !x_max ||
	    y_begin >= !y_max ||
	    y_end >= !y_max)
      then
	raise (Lab_exception "Error: the case beginning or the case ending is invalid.")
    else
	begin
	  Array.set !tcase (x_begin + y_begin * !x_max) (CaseHexa.set_type_case !tcase.((x_begin + y_begin * !x_max)) ENTER);
	  Array.set !tcase (x_end + y_end * !x_max) (CaseHexa.set_type_case !tcase.((x_end + y_end * !x_max)) QUIT);
	  solve_lab_rec (-1) 0 ((x_begin, y_begin), (getValCase (x_begin, y_begin)));
	end


  end


module SquareLab = MakeLabSquare (CaseSquare)
module HexaLab = MakeLabHexa (CaseHexa)

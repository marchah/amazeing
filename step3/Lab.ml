type t = Case.t array

exception Lab_exception of string

let x_max = ref 0
let y_max = ref 0
let tcase = ref [||]


let getValCase y x =
  if (y >= !y_max || x >= !x_max || (y * !x_max + x) >= (!x_max * !y_max)) then
    raise (Lab_exception "Error: getValCase: invalid offset, it's too big!")
  else if (y < 0 || x < 0) then
    raise (Lab_exception "Error: getValCase: invalid offset, it's negative!")
  else
    !tcase.(y * !x_max + x)

let init_lab_case y x =
  if ((y * x) >= Sys.max_array_length) then
    raise (Lab_exception "Error: Array size is too big!")
  else
    Array.init (y * x) (fun _ -> Case.new_case false false false false)

let set_all_case_num num_src num_dest =
  let rec _set_all_case_num =
    function
      | pos when (pos >= (!y_max * !x_max))		->
	()
      | pos when (Case.get_num !tcase.(pos) == num_src) ->
	begin
	  Array.set !tcase pos (Case.set_num !tcase.(pos) num_dest);
	  _set_all_case_num (pos+1)
	end
      | pos						->
	_set_all_case_num (pos+1)
  in _set_all_case_num 0

let check_lab_finish () =
  let save = (Case.get_num !tcase.(0)) in
  let rec _check_lab_finish =
    function
      | pos when (pos >= (!y_max * !x_max))		-> true
      | pos when (save != (Case.get_num !tcase.(pos)))	-> false
      | pos						-> _check_lab_finish (pos+1)
  in _check_lab_finish 0

let make_lab () =
  let rec _make_lab nb y x =
    begin
      if (check_lab_finish () = true) then
	  ()
      else if ((x + 1 < !x_max) && (Case.get_num !tcase.(x + y * !x_max + 1)) != (Case.get_num !tcase.(x + y * !x_max))) then
	begin
	  Array.set !tcase (x + y * !x_max) (Case.set_right !tcase.(x + y * !x_max) true);
	  Array.set !tcase (x + y * !x_max + 1) (Case.set_left !tcase.(x + y * !x_max + 1) true);
	  set_all_case_num (Case.get_num !tcase.(x + y * !x_max + 1)) (Case.get_num !tcase.(x + y * !x_max));
	  _make_lab (nb-1) (Random.int !y_max) (Random.int !x_max)
	end
      else if ((x > 0) && (Case.get_num !tcase.(x + y * !x_max - 1)) != (Case.get_num !tcase.(x + y * !x_max))) then
	begin
	  Array.set !tcase (x + y * !x_max) (Case.set_left !tcase.(x + y * !x_max) true);
	  Array.set !tcase (x + y * !x_max - 1) (Case.set_right !tcase.(x + y * !x_max - 1) true);
	  set_all_case_num (Case.get_num !tcase.(x + y * !x_max - 1)) (Case.get_num !tcase.(x + y * !x_max));
	  _make_lab (nb-1) (Random.int !y_max) (Random.int !x_max)
	end
      else if ((y > 0) && (Case.get_num !tcase.(x + (y - 1) * !x_max)) != (Case.get_num !tcase.(x + y * !x_max))) then
	begin
	  Array.set !tcase (x + y * !x_max) (Case.set_top !tcase.(x + y * !x_max) true);
	  Array.set !tcase (x + (y - 1) * !x_max) (Case.set_bottom !tcase.(x + (y - 1) * !x_max) true);
	  set_all_case_num (Case.get_num !tcase.(x + (y - 1) * !x_max)) (Case.get_num !tcase.(x + y * !x_max));
	  _make_lab (nb-1) (Random.int !y_max) (Random.int !x_max)
	end
      else if ((y + 1 < !y_max) && (Case.get_num !tcase.(x + (y + 1) * !x_max)) != (Case.get_num !tcase.(x + y * !x_max))) then
	begin
	  Array.set !tcase (x + y * !x_max) (Case.set_bottom !tcase.(x + y * !x_max) true);
	  Array.set !tcase (x + (y + 1) * !x_max) (Case.set_top !tcase.(x + (y + 1) * !x_max) true);
	  set_all_case_num (Case.get_num !tcase.(x + (y + 1) * !x_max)) (Case.get_num !tcase.(x + y * !x_max));
	  _make_lab (nb-1) (Random.int !y_max) (Random.int !x_max)
	end
      else
	_make_lab (nb) (Random.int !y_max) (Random.int !x_max)
    end
  in _make_lab (!x_max * !y_max - 1) (Random.int !y_max) (Random.int !x_max)


let init_lab y x =
  begin
    Random.self_init ();
    x_max := x;
    y_max := y;
    tcase := init_lab_case !y_max !x_max;
    make_lab ()
  end


(****************************** AFF *******************************)

let aff_lab screen =
  let rec _aff_lab_y y screen =
    if y < !y_max then
      begin
        let rec _aff_lab_yx y x screen =
          begin
            if x < !x_max then
              begin
                Aff.case (getValCase y x) x y screen;
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


(****************************** SOLVE *******************************)


let solve_lab (x_begin, y_begin) (x_end, y_end) =
  let rec solve_lab_rec come_from nbr = function
    | ((x,y), case) when Case.get_type_case case = Case.QUIT   -> (Array.set !tcase (x + y * !x_max) (Case.set_solve !tcase.(x + y * !x_max) true) ; true)
    | ((x,y), case)                                            ->
	if (come_from != 2 && nbr = 0 && Case.get_top case = true) then
	  match solve_lab_rec 0 0 ((x,y - 1), (getValCase (y - 1) x)) with
	    | true  ->  (Array.set !tcase (x + y * !x_max) (Case.set_solve !tcase.(x + y * !x_max) true) ; true)
	    | false ->  solve_lab_rec come_from (nbr + 1) ((x,y), case)
	else if (come_from != 3 && nbr = 1 && Case.get_right case = true) then
	  match solve_lab_rec 1 0 ((x + 1,y), (getValCase y (x + 1))) with
	    | true  ->  (Array.set !tcase (x + y * !x_max) (Case.set_solve !tcase.(x + y * !x_max) true) ; true)
	    | false ->  solve_lab_rec come_from (nbr + 1) ((x,y), case)
	else if (come_from != 0 && nbr = 2 && Case.get_bottom case = true) then
	  match solve_lab_rec 2 0 ((x,y + 1), (getValCase (y + 1) x)) with
	    | true  ->  (Array.set !tcase (x + y * !x_max) (Case.set_solve !tcase.(x + y * !x_max) true) ; true)
	    | false ->  solve_lab_rec come_from (nbr + 1) ((x,y), case)
	else if (come_from != 1 && nbr = 3 && Case.get_left case = true) then
	  match solve_lab_rec 3 0 ((x - 1,y), (getValCase y (x - 1))) with
	    | true  ->  (Array.set !tcase (x + y * !x_max) (Case.set_solve !tcase.(x + y * !x_max) true) ; true)
	    | false ->  solve_lab_rec come_from (nbr + 1) ((x,y), case)
	else if (nbr < 4) then
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
	Array.set !tcase (x_begin + y_begin * !x_max) (Case.set_type_case !tcase.((x_begin + y_begin * !x_max)) Case.ENTER);
	Array.set !tcase (x_end + y_end * !x_max) (Case.set_type_case !tcase.((x_end + y_end * !x_max)) Case.QUIT);
	solve_lab_rec (-1) 0 ((x_begin, y_begin), (getValCase y_begin x_begin));
      end

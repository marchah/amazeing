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

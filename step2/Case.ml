type case  = ENTER | QUIT | NO

type t = {mutable top : bool; mutable bottom : bool; mutable left : bool; mutable right : bool; mutable num : int; mutable solve : bool; mutable type_case : case}
let count = ref 0

let new_case t b l r = 
  begin
    count := !count + 1;
    {top = t; bottom = b; left = l; right = r; num = !count; solve = false; type_case = NO}
  end

let get_top {top = t; bottom = _; left = _; right = _; num = _; solve = _; type_case = _} = t
let get_bottom {top = _; bottom = b; left = _; right = _; num = _; solve = _; type_case = _} = b
let get_left {top = _; bottom = _; left = l; right = _; num = _; solve = _; type_case = _} = l
let get_right {top = _; bottom = _; left = _; right = r; num = _; solve = _; type_case = _} = r
let get_num {top = _; bottom = _; left = _; right = _; num = n; solve = _; type_case = _} = n
let get_solve {top = _; bottom = _; left = _; right = _; num = _; solve = s; type_case = _} = s
let get_type_case {top = _; bottom = _; left = _; right = _; num = _; solve = _; type_case = c} = c


let set_top {top = _; bottom = b; left = l; right = r; num = n; solve = s; type_case = c} t =
  {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = c}

let set_bottom {top = t; bottom = _; left = l; right = r; num = n; solve = s; type_case = c} b =
  {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = c}

let set_left {top = t; bottom = b; left = _; right = r; num = n; solve = s; type_case = c} l =
  {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = c}

let set_right {top = t; bottom = b; left = l; right = _; num = n; solve = s; type_case = c} r =
  {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = c}

let set_solve {top = t; bottom = b; left = l; right = r; num = n; solve = _; type_case = c} s =
  {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = c}

let set_num {top = t; bottom = b; left = l; right = r; num = _; solve = s; type_case = c} n =
  {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = c}

let set_type_case {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = _} c =
  {top = t; bottom = b; left = l; right = r; num = n; solve = s; type_case = c}

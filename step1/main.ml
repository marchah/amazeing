let main () =
  if (Array.length Sys.argv < 3) then
    print_endline("Usage: ./step y x.")
  else if (int_of_string(Sys.argv.(1)) < 1
	   || int_of_string(Sys.argv.(2)) < 1) then
    print_endline("Invalid size, they must be positive!")
  else
    begin
      try
	Lab.init_lab (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2)));
	Lab.print_lab ();
      with
	| Lab.Lab_exception s	-> print_endline s;
	| _			-> print_endline "Error: Lab parsing."
    end

let _ = main()

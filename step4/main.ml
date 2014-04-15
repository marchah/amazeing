open Sdl
open Sdlvideo
open Sdlloader

let main () =
  if (Array.length Sys.argv < 4) then
    print_endline("Usage: ./step y x --case.")
  else if (int_of_string(Sys.argv.(1)) < 1
              || int_of_string(Sys.argv.(2)) < 1) then
    print_endline("Invalid size, they must be positive!")
  else
    let screen = Aff.init (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(1)) Sys.argv.(3) in
    begin
      try
	if (Sys.argv.(3) = "--square") then
	  begin
	    Functor.SquareLab.init_lab_case (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2));
	    if (Functor.SquareLab.solve_lab (0, 0) ((int_of_string Sys.argv.(2) - 1), (int_of_string Sys.argv.(1) - 1)) = false) then
              print_endline "Failed to solve lab.";
	    Functor.SquareLab.aff_lab screen;
	    Aff.wait screen;
	  end
	else if (Sys.argv.(3) = "--hexagonal") then
	  begin
	    Functor.HexaLab.init_lab_case (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2));
	    if (Functor.HexaLab.solve_lab (0, 0) ((int_of_string Sys.argv.(2) - 1), (int_of_string Sys.argv.(1) - 1)) = false) then
              print_endline "Failed to solve lab.";
	    Functor.HexaLab.aff_lab screen;
	    Aff.wait screen;
	  end
	else
	  print_endline "Error: Invalid type case!"

      with
        | Sdl.SDL_init_exception s        -> print_endline s
        | Sdlloader.SDLloader_exception s -> print_endline s
        | Sdlvideo.Video_exn s            -> print_endline s
        | Sdlevent.Event_exn s            -> print_endline s
        | Functor.Lab_exception s         -> print_endline s
        | _                               -> print_endline "Error: Lab parsing."
    end

let _ = main()

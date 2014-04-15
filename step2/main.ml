open Sdl
open Sdlvideo
open Sdlloader
 
let main () =
  if (Array.length Sys.argv < 3) then
    print_endline("Usage: ./step y x.")
  else if (int_of_string(Sys.argv.(1)) < 1
	   || int_of_string(Sys.argv.(2)) < 1) then
    print_endline("Invalid size, they must be positive!")
  else
    let screen = Aff.init (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(1)) in
    begin
      try
      Lab.init_lab (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2));
      Lab.aff_lab screen;
      Aff.wait screen;
      
      with
	| Sdl.SDL_init_exception s	  -> print_endline s
	| Sdlloader.SDLloader_exception s -> print_endline s
	| Sdlvideo.Video_exn s		  -> print_endline s
	| Sdlevent.Event_exn s		  -> print_endline s
        | Lab.Lab_exception s		  -> print_endline s
	| _				  -> print_endline "Error: Lab parsing."
    end
      
let _ = main()

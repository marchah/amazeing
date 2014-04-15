open Sdlevent
open Sdlkey
open Sdlwm

let init x y case = 
  begin
    Sdl.init [`VIDEO];
    Sdlwm.set_caption "A-maze-ing is amazing :D" "";
    if case = "--square" then
      Sdlvideo.set_video_mode (x * 32) (y * 32) []
    else
      Sdlvideo.set_video_mode (x * 76) (y * 76) []
  end

let rec wait screen =
 match Sdlevent.wait_event () with
   | QUIT			 -> Sdl.quit ()
   | KEYDOWN {keysym=KEY_ESCAPE} -> Sdl.quit ()
   | event 			 ->
     begin
       Sdlvideo.flip screen;
       wait screen
     end
       

open Sdlevent
open Sdlkey
open Sdlwm

let init x y = 
  begin
    Sdl.init [`VIDEO];
    Sdlwm.set_caption "A-maze-ing is amazing :D" "";
    Sdlvideo.set_video_mode (x * 32) (y * 32) []
  end

let case case x y screen =
  let ground = Sdlloader.load_image "../images/ground.png"
  and ground2 = Sdlloader.load_image "../images/ground2.png"
  and wallv = Sdlloader.load_image "../images/wallv.png"
  and wallh = Sdlloader.load_image "../images/wallh.png"
  and pos = Sdlvideo.rect (x * 32) (y * 32) 0 0
  and pos_top = Sdlvideo.rect (x * 32) (y * 32 - 8) 0 0
  and pos_left = Sdlvideo.rect (x * 32 - 8) (y * 32) 0 0
  and pos_bottom = Sdlvideo.rect (x * 32) ((y + 1) * 32 - 8) 0 0
  and pos_right = Sdlvideo.rect ((x + 1) * 32 - 8) (y * 32) 0 0 in
  begin
    begin
      if Case.get_solve case then
	Sdlvideo.blit_surface ~src:ground2 ~dst_rect:pos ~dst:screen ()
      else 
	Sdlvideo.blit_surface ~src:ground ~dst_rect:pos ~dst:screen ()
    end;
    begin
      if Case.get_top case == false then
	Sdlvideo.blit_surface ~src:wallh ~dst_rect:pos_top ~dst:screen ()
    end;
    begin
      if Case.get_bottom case == false then
	Sdlvideo.blit_surface ~src:wallh ~dst_rect:pos_bottom ~dst:screen ()
    end;
    begin
      if Case.get_left case == false then
	Sdlvideo.blit_surface ~src:wallv ~dst_rect:pos_left ~dst:screen ()
    end;
    begin 
      if Case.get_right case == false then
	Sdlvideo.blit_surface ~src:wallv ~dst_rect:pos_right ~dst:screen ()
    end
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
     

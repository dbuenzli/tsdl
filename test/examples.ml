(*---------------------------------------------------------------------------
   Copyright (c) 2024 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Tsdl

let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; 1
    | Ok w ->
        Sdl.pump_events ();
        Sdl.delay 3000l;
        Sdl.destroy_window w;
        Sdl.quit ();
        0

let () = if !Sys.interactive then () else exit (main ())

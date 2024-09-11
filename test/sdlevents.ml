(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Tsdl

let log fmt = Format.printf (fmt ^^ "@.")
let log_err fmt = Format.eprintf (fmt ^^ "@.")

let close_joysticks joysticks = List.iter Sdl.joystick_close joysticks
let open_joysticks () = match Sdl.num_joysticks () with
| Error (`Msg e) -> log_err " Could not get number of joysticks: %s" e; []
| Ok count ->
    let joysticks = ref [] in
    for i = 0 to count - 1 do
      begin match Sdl.joystick_open i with
      | Error (`Msg e) -> log_err " Could not open joystick %d: %s" i e
      | Ok j ->
          joysticks := j :: !joysticks;
          begin match Sdl.joystick_name j with
          | Error (`Msg e) ->
              log_err " Could not get joystick %d's name: %s" i e
          | Ok name -> log "Opened joystick %s" name
          end
      end
    done;
    List.rev !joysticks

let event_loop () =
  let e = Sdl.Event.create () in
  let rec loop () = match Sdl.wait_event (Some e) with
  | Error (`Msg e) -> log_err " Could not wait event: %s" e; ()
  | Ok () ->
      log "%a" Fmts.pp_event e;
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> ()
      | `Drop_file -> Sdl.Event.drop_file_free e; loop ()
      | _ -> loop ()
  in
  Sdl.start_text_input ();
  loop ()

let main () =
  let inits = Sdl.Init.(video + joystick + gamecontroller + events) in
  match Sdl.init inits with
  | Error (`Msg e) -> log_err " SDL init: %s" e
  | Ok () ->
      let flags = Sdl.Window.(shown + mouse_focus + resizable) in
      match Sdl.create_window ~w:640 ~h:480 "SDL events" flags with
      | Error (`Msg e) -> log_err " Create window: %s" e
      | Ok w ->
          let joysticks = open_joysticks () in
          event_loop ();
          close_joysticks joysticks;
          Sdl.destroy_window w;
          Sdl.quit ();
          exit 0

let () = main ()

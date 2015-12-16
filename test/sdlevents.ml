(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   tsdl release 0.9.0
  ---------------------------------------------------------------------------*)

open Tsdl;;
open Result;;

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

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)

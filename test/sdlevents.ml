(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Tsdl;;

let log fmt = Format.printf (fmt ^^ "@.") 
let log_err fmt = 
  let k ppf = Format.printf ": %s@." (Sdl.get_error ()) in
  Format.kfprintf k Format.std_formatter fmt 

let main () = 
  let inits = Sdl.Init.(video + joystick + gamecontroller + events) in
  match Sdl.init inits with 
  | `Error -> log_err " SDL init:"
  | `Ok () ->
      let flags = Sdl.Window.(shown + mouse_focus + resizable) in
      match Sdl.create_window ~w:640 ~h:480 "SDL events" flags with 
      | `Error -> log_err " Create window:"
      | `Ok w -> 
            Sdl.start_text_input ();
            let e = Sdl.Event.create () in
            try 
              while true do match Sdl.wait_event (Some e) with 
              | `Error -> log_err " Could not wait event"; raise Exit
              | `Ok () -> 
                  log "%a" Fmts.pp_event e; 
                  match Sdl.Event.(get e typ) with 
                  | t when t = Sdl.Event.drop_file -> Sdl.Event.drop_file_free e
                  | t when t = Sdl.Event.quit -> raise Exit
                  | _ -> ()
              done;
            with Exit ->
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

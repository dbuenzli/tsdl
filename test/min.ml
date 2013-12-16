(* 
   Minimal OCaml SDL example. This code is in the public domain. 
   Compile with:
   ocamlfind ocamlc -package tsdl -linkpkg -o min.byte min.ml
   ocamlfind ocamlopt -package tsdl -linkpkg -o min.native min.ml
*)

open Tsdl

let log_err fmt = 
  let k msg = Sdl.log "%s: %s@." msg (Sdl.get_error ()) in
  Format.ksprintf k fmt 
    
let main () = match Sdl.init Sdl.Init.video with 
| `Error -> log_err "Init error"; exit 1
| `Ok () -> 
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with 
    | `Error -> log_err "Create window error"; exit 1
    | `Ok w ->
        Sdl.delay 3000l; 
        Sdl.destroy_window w; 
        Sdl.quit (); 
        exit 0
          
let () = main ()

let c_headers = "#define SDL_MAIN_HANDLED 1\n\
                 #include \"SDL.h\"\n\
                 #include \"SDL_vulkan.h\"\n\
                 #undef main"

let concurrency = Cstubs.sequential
let errno = Cstubs.ignore_errno
let prefix = "sdl2_stubs"

let main () =
  match Sys.argv.(1) with
  | "ml" ->
     let ml_out = open_out "src/functions_generated.ml" in
     let ml_fmt = Format.formatter_of_out_channel ml_out in
     Cstubs.write_ml ~concurrency ml_fmt ~prefix (module Function_description.Functions);
     Format.pp_print_flush ml_fmt ();
     close_out ml_out
  | "c" ->
     let c_out = open_out "src/functions_stubs.c" in
     let c_fmt = Format.formatter_of_out_channel c_out in
     Format.fprintf c_fmt "%s@\n" c_headers;
     Cstubs.write_c ~concurrency c_fmt ~prefix (module Function_description.Functions);
     Format.pp_print_flush c_fmt ();
     close_out c_out
  | _ -> ()

let () = main ()

let c_headers = "#include <SDL.h>\n\
                 #define CAML_NAME_SPACE"

let main () =
  let stubs_out = open_out "support/types_stubs_gen.c" in
  let stubs_fmt = Format.formatter_of_out_channel stubs_out in
  Format.fprintf stubs_fmt "%s@\n" c_headers;
  Cstubs.Types.write_c stubs_fmt (module Type_description.Types);
  Format.pp_print_flush stubs_fmt ();
  close_out stubs_out

let () = main ()

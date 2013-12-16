open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support. *)

let pkg_config flags package = 
  let cmd tmp = 
    Command.execute ~quiet:true & 
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  with_temp_file "pkgconfig" "pkg-config" cmd
       
let pkg_config_lib has_lib lib stublib = 
  let tag = Printf.sprintf "use_%s" lib in 
  let ocaml_copts pre_opt l = List.map (fun a -> S [A pre_opt; a]) l in
  let cflags = has_lib :: pkg_config "cflags" lib in 
  let cflags_ocaml = ocaml_copts "-ccopt" cflags in
  let ldflags = pkg_config "libs" lib in
  let ldflags_ocaml = ocaml_copts "-cclib" ldflags in
  let dll_stublib = [ A "-dllib"; A (Printf.sprintf "-l%s" stublib) ] in
  flag ["c"; "ocamlmklib"; tag] (S ldflags);
  flag ["c"; "compile"; tag] (S cflags_ocaml); 
  flag ["link"; "ocaml"; tag] (S ((A "-thread") :: ldflags_ocaml));
  flag ["link"; "ocaml"; "library"; "byte"; tag] (S dll_stublib)
    
(* tsdl_const.ml generation. *)

let sdl_consts_build () =
  dep [ "link"; "ocaml"; "link_consts_stub" ] [ "support/consts_stub.o" ];
  dep [ "sdl_consts" ] [ "src/tsdl_consts.ml" ];
  rule "sdl_consts: consts.byte -> tsdl_consts.ml"
    ~dep:"support/consts.byte" 
    ~prod:"src/tsdl_consts.ml"
    begin fun env build -> 
      let enums = env "support/consts.byte" in 
      let prod = env "src/tsdl_consts.ml" in
      Cmd (S [A enums; A prod])
    end;
;;

let () = 
  dispatch begin function
  | After_rules ->
      pkg_config_lib (A "-DHAS_SDL2") "sdl2" "tsdl";
      sdl_consts_build ()
  | _ -> ()
  end
    

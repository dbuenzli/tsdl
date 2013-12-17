open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support. *)

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let pkg_config flags package = 
  let cmd tmp = 
    Command.execute ~quiet:true & 
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  with_temp_file "pkgconfig" "pkg-config" cmd
       
let pkg_config_lib ~lib ~has_lib ~stublib = 
  let cflags = (A has_lib) :: pkg_config "cflags" lib in
  let ldflags =
    let os_flags = if os = "Linux\n" then [ A "-Wl,-no-as-needed"] else [] in
    os_flags @ pkg_config "libs" lib 
  in
  let ocaml_copts pre_opt l = List.map (fun a -> S [A pre_opt; a]) l in
  let ocaml_cflags = ocaml_copts "-ccopt" cflags in
  let ocaml_ldflags = ocaml_copts "-cclib" ldflags in
  let dll_stublib = [ A "-dllib"; A (Printf.sprintf "-l%s" stublib) ] in
  let tag = Printf.sprintf "use_%s" lib in
  flag ["c"; "ocamlmklib"; tag] (S ldflags);
  flag ["c"; "compile"; tag] (S ocaml_cflags); 
  flag ["link"; "ocaml"; tag] (S ocaml_ldflags);
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
      pkg_config_lib ~lib:"sdl2" ~has_lib:"-DHAS_SDL2" ~stublib:"tsdl";
      sdl_consts_build ()
  | _ -> ()
  end
    

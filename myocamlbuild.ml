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
  let stub_l = match !Ocamlbuild_plugin.Options.ext_lib with
  | "lib" -> [A (Printf.sprintf "dll%s.dll" stublib)]
  | _ -> [A (Printf.sprintf "-l%s" stublib)]
  in
  let libs_l = pkg_config "libs-only-l" lib in
  let libs_L = pkg_config "libs-only-L" lib in
  let libs_other = pkg_config "libs-only-other" lib in
  let mklib_extra = match !Ocamlbuild_plugin.Options.ext_lib with
  | "lib" -> [ S libs_L ]
  | _ -> []
  in
  let linker = match os with
  | "Linux\n" -> [A "-Wl,-no-as-needed"]
  | _ -> []
  in
  let make_opt o arg = S [ A o; arg ] in
  let mklib_flags = (List.map (make_opt "-ldopt") linker)
    @ libs_l @ libs_L @ mklib_extra
  in
  let compile_flags = List.map (make_opt "-ccopt") cflags in
  let lib_flags = List.map (make_opt "-cclib") (libs_l @ libs_other) in
  let link_flags = List.map (make_opt "-ccopt") (linker @ libs_L) in
  let stublib_flags = List.map (make_opt "-dllib") stub_l  in
  let tag = Printf.sprintf "use_%s" lib in
  flag ["c"; "ocamlmklib"; tag] (S mklib_flags);
  flag ["c"; "compile"; tag] (S compile_flags);
  flag ["link"; "ocaml"; tag] (S (link_flags @ lib_flags));
  flag ["link"; "ocaml"; "library"; "byte"; tag] (S stublib_flags)

let obj s =
  match !Ocamlbuild_plugin.Options.ext_obj with
  | "" -> s ^ ".o"
  | x -> s ^ "." ^ x

(* tsdl_const.ml generation. *)

let sdl_consts_build () =
  dep [ "link"; "ocaml"; "link_consts_stub" ] [ obj "support/consts_stub" ];
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

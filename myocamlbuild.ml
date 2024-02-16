open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support. *)

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let pkg_config ?(of_arg=Fun.id) flags package =
  let cmd tmp =
    Command.execute ~quiet:true &
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A (of_arg arg)) (string_list_of_file tmp)
  in
  with_temp_file "pkgconfig" "pkg-config" cmd

let parse_char_option ch opt =
  let len = String.length opt in
  if len <= 2 then
    None
  else
    match opt.[0], opt.[1] with
    | '-', ch -> Some (String.sub opt 2 (len - 2))
    | _, _ -> None

(* Confer: https://github.com/ocaml/dune/issues/119 *)
let msvc_hack_lib =
  match !Ocamlbuild_plugin.Options.ext_lib with
  | "lib" -> fun ~prefix ~ext lib -> (
    (* drop `-l` prefix *)
    match parse_char_option 'l' lib with
    | Some lib' -> prefix ^ lib' ^ ext
    | None -> lib)
  | _ -> fun ~prefix ~ext lib -> lib

let pkg_config_lib ~lib ~has_lib ~stublib =
  let cflags = (A has_lib) :: pkg_config "cflags" lib in
  (* [-l] is expected by pkg-config conventions, but MSVC does not
     support it. Use msvc_hack_lib *)
  let dl_stub_l = [
    A (msvc_hack_lib ~prefix:"dll" ~ext:".dll" (Printf.sprintf "-l%s" stublib))
  ] in
  let libs_l = pkg_config "libs-only-l" lib in
  let cc_libs_l =
    pkg_config
      ~of_arg:(msvc_hack_lib ~prefix:"" ~ext:".lib")
      "libs-only-l"
      lib
  in
  let libs_L = pkg_config "libs-only-L" lib in
  let libs_other = pkg_config "libs-only-other" lib in
  let mklib_extra =
    (* The chain of programs for Windows linking is: ocamlmklib > flexlink > link.
       To get the [libs-only-other] libraries down to `link` it is best to use
       the absolute path to the library. *)
    match !Ocamlbuild_plugin.Options.ext_lib with
    | "lib" ->
      let only_A = function | A s -> Some s | _ -> None in
      let dot_libs =
        List.filter_map only_A libs_other
        |> List.filter (String.ends_with ~suffix:".lib")
      in
      let args =
        List.filter_map only_A libs_L
        |> List.filter_map (parse_char_option 'L')
        |> List.map (fun libdir -> List.map (fun n -> libdir ^ "/" ^ n) dot_libs)
        |> List.flatten
        |> List.filter (Sys.file_exists)
        |> List.map (fun s -> A s)
      in
      [ S args ]
    | _ -> []
  in
  let linker = match os with
  | "Linux\n" -> [A "-Wl,-no-as-needed"]
  | _ -> []
  in
  let make_opt o arg = S [ A o; arg ] in
  let mklib_flags = (List.map (make_opt "-ldopt") linker) @ libs_l @ libs_L @ mklib_extra in
  let compile_flags = List.map (make_opt "-ccopt") cflags in
  let lib_flags = List.map (make_opt "-cclib") (cc_libs_l @ libs_other) in
  let link_flags = List.map (make_opt "-ccopt") (linker @ libs_L) in
  let stublib_flags = List.map (make_opt "-dllib") dl_stub_l  in
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

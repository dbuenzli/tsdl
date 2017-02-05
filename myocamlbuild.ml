open Ocamlbuild_plugin
open Ocb_stubblr

let linker_opts () =
  let tag = "link_no_as_needed" in match os () with
  | `Linux ->
      let linker = "-Wl,-no-as-needed" in
      flag ["c"; "ocamlmklib"; tag] (S [A "-ldopt"; A linker]);
      flag ["link"; "ocaml"; tag] (S [A "-ccopt"; A linker]);
  | _ -> mark_tag_used tag

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
    end

let () = dispatch (
  init & after_rules sdl_consts_build & after_rules linker_opts
)

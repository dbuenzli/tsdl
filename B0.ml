open B0_kit.V000

(* OCaml library names *)

let bigarray_compat = B0_ocaml.libname "bigarray-compat" (* dep of ctypes *)
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let ctypes = B0_ocaml.libname "ctypes"
let ctypes_foreign = B0_ocaml.libname "ctypes-foreign"
let integers = B0_ocaml.libname "integers" (* dep of ctypes *)

let tsdl = B0_ocaml.libname "tsdl"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The tsdl programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/tsdl"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/tsdl/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/tsdl.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/tsdl/issues"
    |> ~~ B0_meta.description_tags
      [ "audio"; "bindings"; "graphics"; "media"; "opengl"; "input"; "hci";
        "org:erratique" ]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.available
      {|[(os-distribution != "opensuse-leap" | os-version >= 16)]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "conf-sdl2", "";
        "ctypes", {|>= "0.21.1"|};
        "ctypes-foreign", {|>= "0.21.1"|} ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.make "default" ~doc:"tsdl package" ~meta ~locked:true @@
  B0_unit.list ()

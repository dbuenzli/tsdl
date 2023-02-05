open B0_kit.V000

(* OCaml library names *)

let ctypes = B0_ocaml.libname "ctypes"
let ctypes_foreign = B0_ocaml.libname "ctypes.foreign"
let integers = B0_ocaml.libname "integers" (* dep of ctypes *)
let bigarray_compat = B0_ocaml.libname "bigarray-compat" (* dep of ctypes *)
let tsdl = B0_ocaml.libname "tsdl"

let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> tag B0_opam.tag
    |> add authors ["The tsdl programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/tsdl"
    |> add online_doc "https://erratique.ch/software/tsdl/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/tsdl.git"
    |> add issues "https://github.com/dbuenzli/tsdl/issues"
    |> add description_tags
      [ "audio"; "bindings"; "graphics"; "media"; "opengl"; "input"; "hci";
        "org:erratique" ]
    |> add B0_opam.Meta.available
      {|[(os-distribution != "opensuse-leap" | os-version >= 16)]|}
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "conf-sdl2", "";
        "ctypes", {|>= "0.14.0"|};
        "ctypes-foreign", "";
      ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
  in
  B0_pack.v "default" ~doc:"tsdl package" ~meta ~locked:true @@
  B0_unit.list ()

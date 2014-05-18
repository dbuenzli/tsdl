#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let () = 
  Pkg.describe "tsdl" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/tsdl";
    Pkg.lib ~exts:Exts.library "src/tsdl_top";
    Pkg.stublibs "src/dlltsdl.so";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/min.ml";
    Pkg.doc "test/minc.c"; ]

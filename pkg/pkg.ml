#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "tsdl" @@ fun c ->
  Ok [
    Pkg.mllib ~api:["Tsdl"] "src/tsdl.mllib";
    Pkg.clib "src/libtsdl_stubs.clib";
    Pkg.mllib ~api:[] "src/top/tsdl_top.mllib";
    Pkg.lib "src/tsdl_top_init.ml";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "test/min.ml";
    Pkg.doc "test/minc.c";
    Pkg.test "test/test_tsdl";
    Pkg.test "test/min";
    Pkg.test "test/examples";
    Pkg.test "test/test_audio";
    Pkg.test "test/test_audio_queue";
    Pkg.test "test/test_audio_capture";
    Pkg.test "test/sdlevents"; ]

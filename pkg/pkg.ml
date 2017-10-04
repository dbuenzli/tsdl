#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "ocb-stubblr.topkg"
open Topkg

let distrib =
  (* FIXME OPAMv2, move this to an x-sdl-version field in the opam file. *)
  let watermarks = ("SDLVERSION", `String "2.0.6") :: Pkg.watermarks in
  Pkg.distrib ~watermarks ()

let build = Pkg.build ~cmd:Ocb_stubblr_topkg.cmd ()

let () =
  Pkg.describe ~build "tsdl" ~distrib @@ fun c ->
  Ok [
    Pkg.mllib ~api:["Tsdl"] "src/tsdl.mllib";
    Pkg.mllib ~api:[] "src/tsdl_top.mllib";
    Pkg.lib "src/tsdl_top_init.ml";
    Pkg.clib "src/libtsdl.clib";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/min.ml";
    Pkg.doc "test/minc.c";
    Pkg.test "test/test";
    Pkg.test "test/min";
    Pkg.test "test/test_audio";
    Pkg.test "test/test_audio_queue";
    Pkg.test "test/test_audio_capture";
    Pkg.test "test/sdlevents"; ]

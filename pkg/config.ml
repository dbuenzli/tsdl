#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default

  let vars =
    [ "NAME", "tsdl";
      "VERSION", Git.describe ~chop_v:true "master";
      "MAINTAINER", "Daniel Bünzli <daniel.buenzl i\\@erratique.ch>";
      "SDLVERSION", "2.0.3"; ]
end

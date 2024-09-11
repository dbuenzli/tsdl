(*---------------------------------------------------------------------------
   Copyright (c) 2013 The tsdl programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Outputs SDL's #defines and enum values as OCaml let bindings. *)

external output_consts : string -> unit = "output_consts"

let main () =
  let outf =
    if Array.length Sys.argv < 2 then "" else
    if Sys.argv.(1) = "" then "" else
    if Sys.argv.(1) = "-" then "" else
    Sys.argv.(1)
  in
  output_consts outf

let () = main ()

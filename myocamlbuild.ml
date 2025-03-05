open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support. *)

let lib_with_clib ~lib ~clib ~has_lib ~src_dir ~stublib =
  let strf = Printf.sprintf in
  let windows = !Ocamlbuild_plugin.Options.ext_lib = "lib" in
  let pkg_config flags package =
    let cmd tmp =
      let pkg_config =
        if not windows then A "pkg-config" else
        S [A "pkg-config"; A "--msvc-syntax"]
      in
      Command.execute ~quiet:true &
      Cmd( S [ pkg_config; A ("--" ^ flags); A package; Sh ">"; A tmp]);
      List.map (fun arg -> A arg) (string_list_of_file tmp)
    in
    with_temp_file "pkgconfig" "pkg-config" cmd
  in
  let ar s = match !Ocamlbuild_plugin.Options.ext_lib with
  | "" -> s ^ ".a" | x -> s ^ "." ^ x
  in
  let make_opt o arg = S [ A o; arg ] in
  let ccopts = List.map (make_opt "-ccopt") in
  let cclibs = List.map (make_opt "-cclib") in
  let dllibs = List.map (make_opt "-dllib") in
  let use_lib = strf "use_%s" lib in
  let use_clib = strf "use_%s" clib in
  let record_stub_lib = strf "record_%s" stublib in
  let link_stub_archive = strf "link_%s_archive" stublib in
  let stub_ar = ar (strf "%s/lib%s" src_dir stublib) in
  let static_stub_l =
    if windows then A (strf "lib%s.lib" stublib) else A (strf "-l%s" stublib)
  in
  let dynamic_stub_l =
    if windows then A (strf "dll%s.dll" stublib) else static_stub_l
  in
  let clib_l = pkg_config "libs-only-l" clib in
  let clib_L =
    let dashldify = function
    | A l when windows -> A (String.subst "/libpath:" "-L" l)
    | arg -> arg
    in
    List.map dashldify (pkg_config "libs-only-L" clib)
  in
  let clib_cflags = ccopts @@ (A has_lib) :: pkg_config "cflags" clib in
  let clib_cclibs = cclibs @@ static_stub_l :: clib_l in
  let clib_ccopts = ccopts @@ clib_L in
  begin
    dep [record_stub_lib] [stub_ar];

    flag ["c"; "compile"; use_clib] (S clib_cflags);

    flag ["c"; "ocamlmklib"; use_clib] (S (clib_L @ clib_l));

    flag ["link"; "ocaml"; "library"; "byte"; record_stub_lib]
      (S (dllibs [dynamic_stub_l] @ clib_ccopts @ clib_cclibs));

    flag ["link"; "ocaml"; "library"; "native"; record_stub_lib]
      (S (clib_ccopts @ clib_cclibs));

    flag_and_dep ["link"; "ocaml"; link_stub_archive] (P stub_ar);

    flag ["link"; "ocaml"; "library"; "shared"; link_stub_archive]
      (S (clib_ccopts @ clib_cclibs));

    ocaml_lib ~tag_name:use_lib ~dir:src_dir (strf "%s/%s" src_dir lib)
  end

let ctypes_stub_gen () =
  (* Types stubs generators *)
  (* Generate stubs. Steps 1, 2, & 3 of Makefile (1 & 2 via built-in rules).
     ML -> C *)
  rule "cstubs: x_c_gen.native -> x_stubs_gen.c"
    ~dep:"%_c_gen.native"
    ~prod:"%_stubs_gen.c"
    (fun env _build -> Cmd (A (env "./%_c_gen.native")));

  (* Step 4. OCamlbuild (nor ocamlc/ocamlopt) has a built in rule for
     linking executables from C. Call out to 'cc'. *)
  rule "stub_gen 1: x_stubs_gen.o -> x_stubs_gen"
    ~dep:"%_stubs_gen.o"
    ~prod:"%_stubs_gen"
    (fun env _build ->
      Cmd (S [ A "cc"; A "-o"; A (env "%_stubs_gen"); A (env "%_stubs_gen.o") ]));

  (* Step 5. Generate ml stubs.  C -> ML  *)
  rule "stubs_gen 2: x_stubs_gen -> x_stubs.ml"
    ~dep:"support/%_stubs_gen"
    ~prod:"src/%_stubs.ml"
    (fun env _build -> Cmd (S[A (env "support/%_stubs_gen"); Sh">"; A (env "src/%_stubs.ml")]));

  (* Functions stubs generators *)
  rule "cstubs functions: x_generator.native -> x_stubs.c"
    ~dep:"support/%_generator.native"
    ~prod:"src/%_stubs.c"
    (fun env _build -> Cmd (S[A (env "./support/%_generator.native"); A "c"]));

 rule "mlstubs functions: x_generator.native -> x_generated.ml"
    ~dep:"support/%_generator.native"
    ~prod:"src/%_generated.ml"
    (fun env _build -> Cmd (S[A (env "./support/%_generator.native"); A "ml"]))

let () =
  dispatch begin function
  | After_rules ->
      lib_with_clib ~lib:"tsdl" ~clib:"sdl2" ~has_lib:"-DHAS_SDL2"
        ~src_dir:"src" ~stublib:"tsdl_stubs";
      ctypes_stub_gen ()
  | _ -> ()
  end

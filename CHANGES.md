v0.9.6 2017-12-27 La Forclaz (VS)
---------------------------------

- Add full support for 2.0.{4,5,6}. Thanks to Florian Angeletti who
  made all the work.
- Add Sdl.Init.nothing. Thanks to @sanette for the patch.

v0.9.5 2017-07-24 Zagreb
------------------------

- Support for audio queue API. Thanks to Erik Løvlie for the patch.

v0.9.4 2017-07-08 Zagreb
------------------------

- Fix linking issues on Linux (#38)

v0.9.3 2017-05-03 La Forclaz (VS)
---------------------------------

- Fix segfaulting `Sdl.load_raw_rw`. Thanks to @sanette for the
  report and the patch.
- Fix audio callback support. The client could not get a handle on the
  actual callback closure which would lead to
  `Ctypes_ffi_stubs.CallToExpiredClosure.` exceptions. The callback
  wrapping is now done via the `Sdl.audio_callback` function. The
  client must keep a reference on the returned value until no longer
  needed.  As a side effect this changes the signature of
  `Sdl.load_raw_rw`. Thanks to @sanette for the report.
- Fix signature of `Sdl.blit_scaled`. Thanks to Léo Andrès for the report
  and the patch.

v0.9.2 2016-12-07 Cambridge (UK)
--------------------------------

- Safe-string support.
- Build depend on `ocb-stubblr`.
- Add support for 2.0.{4,5} events and make the stubs more robust
  to additions of events.

v0.9.1 2016-09-27 Zagreb
------------------------

- Release runtime lock on `Sdl.delay`.
- Reinstate support for audio callbacks, `ocaml-ctypes`
  is getting smarter (#13).
- Really fix signature of `Sdl.blit_surface`, thanks to
  Richard Davison for the report (#25).
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v0.9.0 2015-11-30 Cambridge (UK)
--------------------------------

- Switch from a polymorphic variant result type to the one exposed by
  the `result` compatibility package. This is an incompatible
  change. Thanks to Florian Angeletti for the patch.


v0.8.2 2015-11-20 Cambridge (UK)
--------------------------------

- ctypes >= 0.4.0 support.
- Remove support for audio callback, leads to segfaults.
  See issue #13 for more information.
- Release the OCaml runtime lock on `Sdl.wait_event[_timeout]`.
- Fix constant error messages that shouldn't be. Thanks
  to Vũ Ngọc San for the report.
- Fix signature of `Sdl.blit_surface`, thanks to Frederic
  Bour for the report.
- Change toplevel scheme. Require `tsdl.top` instead of `tsdl` for toplevel
  support.
- Add `Sdl.Color.set_{r,g,b,a}`, `Point.set_{x,y}` and
  `Color.set_{x,y,w,h}`. Thanks to Julian Squires for the patch.
- Add a few new, undocumented `unsafe_*_of_ptr` conversion functions.
  This allows to develop bindings to other C libraries that manipulate
  SDL structures. Thanks to Frederic Bour and Julian Squires for
  the patches.


v0.8.1 2014-05-22 La Forclaz (VS)
---------------------------------

- Fix compilation on 32 bits platforms. 
- Support for ctypes 0.3. Thanks to Jeremy Yallop for the patch.


v0.8.0 2014-05-19 La Forclaz (VS)
---------------------------------

First release.
Part of the work was sponsored by OCaml Labs.

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

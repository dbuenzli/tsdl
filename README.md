Tsdl — Thin bindings to SDL for OCaml
=====================================

Tsdl is an OCaml library providing thin bindings to the cross-platform
[SDL library].

Tsdl depends on the C library SDL 2.0.18 (or later),
[ocaml-ctypes][ctypes]. Tsdl is distributed under the ISC license.

[SDL library]: https://www.libsdl.org/
[ctypes]: https://github.com/ocamllabs/ocaml-ctypes

Home page: <http://erratique.ch/software/tsdl>  


## Installation

Tsdl needs the C library SDL 2.0.18 or later installed on your
system. Tsdl can be installed with `opam`:

    opam install tsdl

If you don't use `opam` consult the [`opam`](opam) file for
build instructions and a complete specification of the dependencies.


## Documentation

The documentation can be consulted [online] or via `odig doc tsdl`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[ocaml-forum]: https://discuss.ocaml.org/
[online]: https://erratique.ch/software/tsdl/doc/

## Sample programs

Sample programs are located in the `test` directory of the
distribution. They can be built with:

    ./pkg/pkg.ml build --tests true

and listed with

    ./pkg/pkg.ml test --list

The resulting binaries are in `_build/test` :

- `test_tsdl.native`, tests the bindings, the executable should exit with 0.
- `sdlevents.native`, traces SDL events.
- `min.native` a minimal SDL example.

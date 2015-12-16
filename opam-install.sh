#!/bin/bash -e

VERSION=0.9.0

orig=$(pwd)
installed=$(opam list | egrep "tsdl +$VERSION")

if [ "A${installed}B" == "AB" ]
  then 
    echo "Installing upstream tsdl..."
    opam install tsdl
fi

echo "Updating tsdl opam install..."
libs=$(opam config var lib)
rm  -f $libs/stublibs/dlltsdl.so*
rm -rf $libs/tsdl
cd _build/src
ocamlfind install tsdl ../pkg/META *.a *.cm[iatx] *.cmx[as] *.cmti *.mli *.so
cd $orig
echo "Done."

exit

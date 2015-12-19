all:
	./pkg/build.ml native=true native-dynlink=true
clean:
	rm -rf _build
install:
	./opam-install.sh

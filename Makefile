.PHONY: test check

bisect:
	rm -f *.coverage
	dune exec --instrument-with bisect_ppx test/main.exe
	bisect-ppx-report html

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f Cornell-Monopoly.zip
	zip -r Cornell-Monopoly.zip . 

clean:
	dune clean
	rm -f *.coverage
	rm -f Cornell-Monopoly.zip

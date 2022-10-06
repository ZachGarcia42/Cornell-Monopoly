.PHONY: test check

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
	OCAMLRUNPARAM=b dune exec src/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f Cornell-Monopoly.zip
	zip -r Cornell-Monopoly.zip . -x@exclude.lst

clean:
	dune clean
	rm -f Cornell-Monopoly.zip

default:
	opam install . --deps-only
	dune build

install:
	opam install --yes . --deps-only

clean:
	dune clean
	git clean -dfX

format:
	dune build @fmt --auto-promote
hook:
	cp ./hooks/* .git/hooks

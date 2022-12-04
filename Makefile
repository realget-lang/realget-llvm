default:
	opam install . --deps-only
	dune build

install:
	opam install --yes . --deps-only

test:
	dune runtest 

clean:
	dune clean
	git clean -dfX

format:
	make pre-build
	dune build @fmt --auto-promote
hook:
	cp ./hooks/* .git/hooks

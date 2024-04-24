.PHONY: default deps run all clean

default: run

deps: 
	opam install . --deps-only --yes

run: 
	dune build --profile release

all: deps runner

clean:
	dune clean
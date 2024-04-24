# How to compile

ocamlc -o server server.ml

# run with
./server


# how to use dune
#building
dune build --profile release

#running 
_build/default/server.exe

# Install dependencies with
opam install . --deps-only --with-test --with-doc


#install dependencies with

opam install . --deps-only

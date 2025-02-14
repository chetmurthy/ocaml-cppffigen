
PACKAGES=camlp5,pa_ppx.deriving_plugins.sexp,sexplib,cmdliner
SYNTAX=camlp5o

OCAMLCFLAGS= -package $(PACKAGES) -syntax $(SYNTAX) -g

all: cppffigen cppffigen_example

cppffigen: cppffigen.ml cppffigen_main.ml
	ocamlfind ocamlc $(OCAMLCFLAGS) -linkall -linkpkg -o cppffigen $^

cppffigen_example: cppffigen.ml cppffigen_example.ml
	ocamlfind ocamlc $(OCAMLCFLAGS) -linkall -linkpkg -o cppffigen_example $^

test: all
	make -C examples/ex1 clean all test

clean::
	rm -f *.cm* cppffigen cppffigen_example
	make -C examples/ex1 clean

install: all
	ocamlfind install cppffigen META cppffigen cppffi.inc cppffi.h

uninstall::
	ocamlfind remove cppffigen



PACKAGES=camlp5,pa_ppx.deriving_plugins.sexp,sexplib,cmdliner,pa_ppx.utils
SYNTAX=camlp5o

OCAMLCFLAGS= -package $(PACKAGES) -syntax $(SYNTAX) -g
YAWRAP=ocamlfind camlp5-buildscripts/ya-wrap-ocamlfind
OCAMLC=$(YAWRAP) ocamlfind ocamlc

all: cppffigen cppffigen_example

cppffigen: cppffigen.cmo cppffigen_main.cmo
	$(OCAMLC) $(OCAMLCFLAGS) -linkall -linkpkg -o cppffigen $^

cppffigen_example: cppffigen.cmo cppffigen_example.cmo
	$(OCAMLC) $(OCAMLCFLAGS) -linkall -linkpkg -o cppffigen_example $^

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

test: all
	make -C examples/ex1 clean all test

clean::
	rm -f *.cm* cppffigen cppffigen_example
	make -C examples/ex1 clean

install: all
	ocamlfind install cppffigen META cppffigen cppffi.inc cppffi.h

uninstall::
	ocamlfind remove cppffigen

.SUFFIXES: .cmo .cmi .ml .mll .mly .mli .cmx .cma .cmxa .cmt .cmti

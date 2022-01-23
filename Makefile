
all: cppffigen cppffigen_example

cppffigen: cppffigen.ml cppffigen_main.ml
	ocamlfind ocamlc -package ppx_sexp_conv,pcre,sexplib,cmdliner -linkall -linkpkg -o cppffigen $^

cppffigen_example: cppffigen.ml cppffigen_example.ml
	ocamlfind ocamlc -package ppx_sexp_conv,pcre,sexplib,cmdliner -linkall -linkpkg -o cppffigen_example $^


clean::
	rm -f *.cm* cppffigen cppffigen_example

install: all
	ocamlfind install cppffigen META cppffigen cppffi.inc cppffi.h

uninstall::
	ocamlfind remove cppffigen


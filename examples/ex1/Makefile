
OCAMLLIB := $(shell ocamlc -where)
OCAMLMKLIB_FLAGS= -lstdc++

RESULT=ocaml_somecode

CXXFLAGS += -g2 -I. -I../.. -I$(OCAMLLIB) -std=gnu++11 -fno-rtti

PACKS = result,threads,oUnit

MLI= $(RESULT).mli
ML=  $(RESULT).ml some_code.ml
CMO= $(patsubst %.ml,%.cmo, $(ML))
CMX= $(patsubst %.ml,%.cmx, $(ML))
CMI= $(patsubst %.ml,%.cmi, $(ML))
OBJECTS = $(CMO) $(CMX) $(CMI)

all: $(RESULT).cma $(RESULT).cmxa dll$(RESULT).so somecode_tests.byte

test: somecode_tests.byte
	mkdir -p _build
	./somecode_tests.byte

somecode_tests.byte: $(RESULT).cma somecode_tests.cmo
	ocamlfind ocamlc -custom -thread -package $(PACKS) -linkpkg -linkall -o somecode_tests.byte $(RESULT).cma -cclib -L.  somecode_tests.cmo

$(RESULT).cma $(RESULT).cmxa dll$(RESULT).so: $(OBJECTS) $(RESULT)_stubs.o somecode.o
	    ocamlmklib -verbose -o $(RESULT) $(CMO) $(CMX) $(RESULT)_stubs.o somecode.o $(OCAMLMKLIB_FLAGS)

somecode_tests.cmo : somecode_tests.ml
	ocamlfind ocamlc -thread -package $(PACKS) -c somecode_tests.ml

$(CMO) $(CMI): $(ML)
	ocamlfind ocamlc -thread -package $(PACKS) -c $(MLI)
	ocamlfind ocamlc -thread -package $(PACKS) -c $(ML)

$(CMX): $(ML) $(CMI)
	ocamlfind ocamlopt -thread -package $(PACKS) -c $(ML)

$(RESULT).ml $(RESULT).mli $(RESULT)_stubs.cc: somecode-ffi.idl
	../../cppffigen --output ml < somecode-ffi.idl > $(RESULT).ml
	../../cppffigen --output mli < somecode-ffi.idl > $(RESULT).mli
	../../cppffigen --output cpp < somecode-ffi.idl > $(RESULT)_stubs.cc

$(RESULT)_stubs.o: $(RESULT)_stubs.cc
	g++ -c -fPIC ${CXXFLAGS} -DPIC -o $(RESULT)_stubs.o $(RESULT)_stubs.cc

somecode.o: somecode.cc
	g++ -c -fPIC ${CXXFLAGS} -DPIC -o $@ $<

clean::
	rm -rf somecode_tests.byte \
		META *.a *.cma *.cmi *.cmo *.cmx *.cmxa *.o *.so *.log *.cache _build \
		$(RESULT).ml $(RESULT).mli $(RESULT).top \
		$(RESULT)_stubs.c $(RESULT)_stubs.c.ORIG $(RESULT)_stubs.cc

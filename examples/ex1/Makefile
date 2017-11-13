
ROCKS_VERSION ?= 5.8
ROCKS_INSTALL ?= $(THRIFTROOT)
ROCKS_LIBDIR ?= $(ROCKS_INSTALL)/lib
ROCKS_LIB ?= rocksdb
export ROCKS_LIB ROCKS_LIBDIR ROCKS_INSTALL

ifeq ($(ROCKS_VERSION),myrocks)
  UNIFDEF_ARGS = -D ROCKS_VERSION_MYROCKS
else
  UNIFDEF_ARGS = -U ROCKS_VERSION_MYROCKS
endif

OCAMLLIB := $(shell ocamlc -where)
OCAMLMKLIB_FLAGS= \
  -Wl,-rpath=$(ROCKS_LIBDIR) -L$(ROCKS_LIBDIR) -l$(ROCKS_LIB) \
  -lstdc++

RESULT=ocaml_rocksdb
RESULT_PACKAGE=ocaml-rocksdb

CXXFLAGS += -g2 -I. -I`ocamlfind query cppffigen` -I$(OCAMLLIB) -I$(ROCKS_INSTALL)/include -std=gnu++11 -fno-rtti

PACKS = result,threads,core_kernel,oUnit

MLI=rocks_types.mli $(RESULT).mli rocks.mli
ML= misc.ml rocks_types.ml $(RESULT).ml rocks.ml
CMO= $(patsubst %.ml,%.cmo, $(ML))
CMX= $(patsubst %.ml,%.cmx, $(ML))
CMI= $(patsubst %.ml,%.cmi, $(ML))
OBJECTS = $(CMO) $(CMX) $(CMI)

all: $(RESULT).cma $(RESULT).cmxa dll$(RESULT).so rocks_tests.byte

test: rocks_tests.byte
	./rocks_tests.byte

rocks_tests.byte: $(RESULT).cma rocks_tests.cmo
	ocamlfind ocamlc -custom -thread -package $(PACKS) -linkpkg -linkall -o rocks_tests.byte $(RESULT).cma -cclib -L.  rocks_tests.cmo

$(RESULT).cma $(RESULT).cmxa dll$(RESULT).so: $(OBJECTS) $(RESULT)_stubs.o somecode.o
	    ocamlmklib -verbose -o $(RESULT) $(CMO) $(CMX) $(RESULT)_stubs.o somecode.o $(OCAMLMKLIB_FLAGS)

rocks_tests.cmo : rocks_tests.ml
	ocamlfind ocamlc -thread -package $(PACKS) -c -g rocks_tests.ml

$(CMO) $(CMI): $(ML)
	ocamlfind ocamlc -thread -package $(PACKS) -c -g $(MLI)
	ocamlfind ocamlc -thread -package $(PACKS) -c -g $(ML)

$(CMX): $(ML) $(CMI)
	ocamlfind ocamlopt -thread -package $(PACKS) -c $(ML)

$(RESULT).ml $(RESULT).mli $(RESULT)_stubs.cc: rocksdb-ffi.idl
	cppffigen --output ml < rocksdb-ffi.idl > $(RESULT).ml
	cppffigen --output mli < rocksdb-ffi.idl > $(RESULT).mli
	cppffigen --output cpp < rocksdb-ffi.idl > $(RESULT)_stubs.cc

$(RESULT)_stubs.o: $(RESULT)_stubs.cc
	g++ -c -fPIC ${CXXFLAGS} -DPIC -o $(RESULT)_stubs.o $(RESULT)_stubs.cc

somecode.o: somecode.cc
	g++ -c -fPIC ${CXXFLAGS} -DPIC -o $@ $<

clean::
	rm -f rocks_tests.byte \
		META *.a *.cma *.cmi *.cmo *.cmx *.cmxa *.o *.so \
		$(RESULT).ml $(RESULT).mli $(RESULT).top \
		$(RESULT)_stubs.c $(RESULT)_stubs.c.ORIG $(RESULT)_stubs.cc $(RESULT_PACKAGE).top \
		rocksffi.ml rocksffi.cc rocksffi.o

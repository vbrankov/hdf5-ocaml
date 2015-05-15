CAMLDEP=ocamldep.opt
CAMLC=ocamlc.opt
CAMLOPT=ocamlopt.opt
MKLIB=ocamlmklib

include ../Makefile.config

CFLAGS=-I $(HDF5_H_DIR) -ccopt "-Wall -pedantic -Wextra -Wunused -Werror \
  -Wno-long-long -std=c99 -DCAML_NAME_SPACE -fPIC"

LIBNAME=hdf5_caml
FOR_PACK_NAME=Hdf5_caml
SRC_C=$(wildcard *.c)

.SUFFIXES: .ml .mli .cmi .cmo .cmx .cmxa .o

all: $(LIBNAME).cmxa

include .depend

$(LIBNAME).cmx: $(SRC_ML:.ml=.cmx)
	$(CAMLOPT) -pack -o $(LIBNAME).cmx $^

$(LIBNAME).cmxa: $(SRC_C:.c=.o) $(LIBNAME).cmx
	$(MKLIB) -ocamlc $(CAMLC) -ocamlopt $(CAMLOPT) -o $(LIBNAME) -oc $(LIBNAME) \
	  -l$(HDF5_LIB) -l$(HDF5_HL_LIB) $^

depend:
	gcc -I $(HDF5_H_DIR) -MM *.c > .depend
	$(CAMLDEP) *.mli *.ml >> .depend
	echo -n SRC_ML= >> .depend
	$(CAMLDEP) -sort *.ml >> .depend

clean::
	rm -f *.cm* *.a *.lib *.o *.so *.omc

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -for-pack $(FOR_PACK_NAME) $(COMPFLAGS) -c $<

.c.o:
	$(CAMLOPT) $(CFLAGS) -c $<

.PHONY: all clean depend

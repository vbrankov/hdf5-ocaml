CAMLDEP=ocamldep
CAMLOPT=ocamlopt
MKLIB=ocamlmklib

CFLAGS=-I /usr/local/hdf5/include -ccopt "-Wall -pedantic -Wextra -Wunused -Werror \
  -Wno-long-long -std=c99 -DCAML_NAME_SPACE -fPIC"

LIBNAME=hdf5_caml
FOR_PACK_NAME=Hdf5_caml
SRC=$(wildcard *.mli)
SRC_C=$(wildcard *.c)

.SUFFIXES: .ml .mli .cmi .cmo .cmx .cmxa .o

all: $(LIBNAME).cmxa

$(LIBNAME).cmx: $(SRC:.mli=.cmx)
	$(CAMLOPT) $^ -pack -o $(LIBNAME).cmx

$(LIBNAME).cmxa: $(LIBNAME).cmx $(SRC_C:.c=.o)
	$(MKLIB) -L/usr/local/hdf5/lib -lhdf5 $^ -o $(LIBNAME)

depend:
	gcc -I /usr/local/hdf5/include -MM *.c > .depend
	$(CAMLDEP) *.mli *.ml >> .depend

include .depend

clean::
	rm -f *.cm* *.a *.lib *.o *.so *.omc

.mli.cmi:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -for-pack $(FOR_PACK_NAME) -c $(COMPFLAGS) $<

.c.o:
	$(CAMLOPT) $(CFLAGS) -c $<

.PHONY: all depend clean

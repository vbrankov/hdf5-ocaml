CAMLDEP=ocamldep.opt
CAMLC=ocamlc.opt
CAMLOPT=ocamlopt.opt

CFLAGS=-I /usr/local/hdf5/include -ccopt "-Wall -pedantic -Wextra -Wunused -Werror \
  -Wno-long-long -std=c99 -DCAML_NAME_SPACE -fPIC"

LIBNAME=hdf5_caml
FOR_PACK_NAME=Hdf5_caml
SRC=$(wildcard *.mli)
SRC_C=$(wildcard *.c)

.SUFFIXES: .ml .mli .cmi .cmo .cmx .cmxa .o

all: $(LIBNAME).cmxa

$(LIBNAME).cmx: $(SRC:.mli=.cmx) $(SRC_C:.c=.o)
	$(CAMLOPT) -pack -o $(LIBNAME).cmx $^

$(LIBNAME).cmxa: $(LIBNAME).cmx
	$(CAMLOPT) $^ -a -o $(LIBNAME).cmxa

depend:
	gcc -I /usr/local/hdf5/include -MM *.c > .depend
	$(CAMLDEP) *.mli *.ml >> .depend

include .depend

clean::
	rm -f *.cm* *.a *.lib *.o *.so *.omc

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) $<

.ml.cmx: $(SRC:.mli=.cmi)
	$(CAMLOPT) -c -for-pack $(FOR_PACK_NAME) $(COMPFLAGS) $<

.c.o:
	$(CAMLOPT) $(CFLAGS) -c $<

.PHONY: all depend clean

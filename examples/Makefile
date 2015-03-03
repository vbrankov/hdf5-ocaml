CAMLDEP=ocamldep.opt
CAMLOPT=ocamlopt.opt

COMPFLAGS=-I ../lib
LINKFLAGS=$(COMPFLAGS) hdf5_caml.cmxa
TARGETS=test.exe

.SUFFIXES: .exe .ml .mli .cmi .cmx

all: $(TARGETS)

.ml.exe:
	$(CAMLOPT) $(LINKFLAGS) -o $@ $<

clean::
	rm -f *.cm* *.exe *.o

depend:
	$(CAMLDEP) *.mli *.ml > .depend

.PHONY: all depend clean

include .depend

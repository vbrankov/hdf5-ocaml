CAMLDEP=ocamldep
CAMLOPT=ocamlopt

COMPFLAGS=-I ../lib
LINKFLAGS=$(COMPFLAGS) hdf5_caml.cmxa
TARGETS=test.exe

.SUFFIXES: .exe .ml .mli .cmi .cmx

all: depend $(TARGETS)

.cmx.exe:
	$(CAMLOPT) $(LINKFLAGS) -o $@ $<

.mli.cmi:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

clean::
	rm -f *.cm* *.exe *.o

depend:
	$(CAMLDEP) *.mli *.ml > .depend

.PHONY: all depend clean

include .depend

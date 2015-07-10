CAMLDEP=ocamldep.opt
CAMLOPT=ocamlopt.opt

include ../../Makefile.config

COMPFLAGS=-I $(COMPILER_LIBS)
LINKFLAGS=$(COMPFLAGS) ocamlcommon.cmxa

.SUFFIXES: .exe .ml .mli .cmi .cmx

all: ppx_h5struct.exe

.ml.exe:
	$(CAMLOPT) $(LINKFLAGS) -o $@ $<

clean::
	rm -f *.cm* *.exe *.o core.*

.PHONY: all clean

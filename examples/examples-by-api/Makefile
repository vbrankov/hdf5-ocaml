include ../Makefile.shared

test: $(TARGETS)
	@for i in ${^:.exe=}; do \
	  ./$$i.exe > $$i.out && \
	  diff -q $$i.out $$i.tst && \
	  rm $$i.out; \
	done

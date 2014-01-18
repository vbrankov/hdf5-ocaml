all: subdirs

subdirs:
	for dir in lib examples; do \
          $(MAKE) -C $$dir; \
        done

clean:
	for dir in lib examples; do \
          $(MAKE) -C $$dir clean; \
        done

.PHONY: all clean subdirs

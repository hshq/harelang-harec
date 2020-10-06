.POSIX:
.SUFFIXES:
OUTDIR=.build
include $(OUTDIR)/config.mk
include $(OUTDIR)/cppcache

harec: $(harec_objects)
	@printf 'CCLD\t$@\n'
	@$(CC) $(LDFLAGS) -o $@ $(harec_objects) $(LIBS)

.SUFFIXES: .c .o .scd .1 .5

.c.o:
	@printf 'CC\t$@\n'
	@touch $(OUTDIR)/cppcache
	@grep $< $(OUTDIR)/cppcache >/dev/null || \
		$(CPP) $(CFLAGS) -MM -MT $@ $< >> $(OUTDIR)/cppcache
	@$(CC) -c $(CFLAGS) -o $@ $<

docs:

clean:
	@rm -f harec $(harec_objects)

distclean: clean
	@rm -rf "$(OUTDIR)"

install:

.PHONY: docs clean distclean install

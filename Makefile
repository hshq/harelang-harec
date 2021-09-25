.POSIX:
.SUFFIXES:
OUTDIR=.build
include $(OUTDIR)/config.mk
include $(OUTDIR)/cppcache

harec: $(harec_objects)
	@printf 'CCLD\t$@\n'
	@$(CC) $(LDFLAGS) -o $@ $(harec_objects) $(LIBS)

include types/Makefile
include rt/Makefile
include tests/Makefile

.SUFFIXES: .c .o .ha .s .scd .1 .5

.c.o:
	@printf 'CC\t$@\n'
	@touch $(OUTDIR)/cppcache
	@grep $< $(OUTDIR)/cppcache >/dev/null || \
		$(CPP) $(CFLAGS) -MM -MT $@ $< >> $(OUTDIR)/cppcache
	@$(CC) -c $(CFLAGS) -o $@ $<

.s.o:
	@printf 'AS\t$@\n'
	@$(AS) -o $@ $<

docs:

clean: clean-tests clean-rt
	@rm -f harec $(harec_objects)

distclean: clean
	@rm -rf "$(OUTDIR)"

install: harec
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m755 harec $(DESTDIR)$(BINDIR)/harec

.PHONY: docs clean distclean install

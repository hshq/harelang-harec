.POSIX:

all:

include config.mk
include makefiles/$(PLATFORM).mk
include makefiles/tests.mk

all: $(BINOUT)/harec

C_DEFINES = \
	-DVERSION='"'"$(VERSION)"'"' \
	-DDEFAULT_TARGET='"$(DEFAULT_TARGET)"'

headers = \
	include/ast.h \
	include/check.h \
	include/emit.h \
	include/eval.h \
	include/expr.h \
	include/gen.h \
	include/identifier.h \
	include/lex.h \
	include/mod.h \
	include/parse.h \
	include/qbe.h \
	include/scope.h \
	include/type_store.h \
	include/typedef.h \
	include/types.h \
	include/utf8.h \
	include/util.h

harec_objects = \
	src/check.o \
	src/emit.o \
	src/eval.o \
	src/gen.o \
	src/genutil.o \
	src/identifier.o \
	src/lex.o \
	src/main.o \
	src/mod.o \
	src/parse.o \
	src/qbe.o \
	src/qinstr.o \
	src/qtype.o \
	src/scope.o \
	src/type_store.o \
	src/typedef.o \
	src/types.o \
	src/utf8.o \
	src/util.o

$(BINOUT)/harec: $(harec_objects)
	@mkdir -p -- $(BINOUT)
	@printf 'CCLD\t%s\n' '$@'
	@$(CC) $(LDFLAGS) -o $@ $(harec_objects) $(LIBS)

.SUFFIXES:
.SUFFIXES: .ha .ssa .td .c .o .s .scd .1 .5

src/check.o: $(headers)
src/emit.o: $(headers)
src/eval.o: $(headers)
src/gen.o: $(headers)
src/genutil.o: $(headers)
src/identifier.o: $(headers)
src/lex.o: $(headers)
src/main.o: $(headers)
src/mod.o: $(headers)
src/parse.o: $(headers)
src/qbe.o: $(headers)
src/qinstr.o: $(headers)
src/qtype.o: $(headers)
src/scope.o: $(headers)
src/type_store.o: $(headers)
src/typedef.o: $(headers)
src/types.o: $(headers)
src/utf8.o: $(headers)
src/util.o: $(headers)

.c.o:
	@printf 'CC\t%s\n' '$@'
	@$(CC) -c $(CFLAGS) $(C_DEFINES) -o $@ $<

.s.o:
	@printf 'AS\t%s\n' '$@'
	@$(AS) $(ASFLAGS) -o $@ $<

.ssa.s:
	@printf 'QBE\t%s\n' '$@'
	@$(QBE) $(QBEFLAGS) -o $@ $<

.ssa.td:
	@cmp -s $@ $@.tmp 2>/dev/null || cp $@.tmp $@

.ha.ssa:
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $<

clean:
	@rm -rf -- $(HARECACHE) $(BINOUT) $(harec_objects) $(tests)

check: $(BINOUT)/harec $(tests)
	@./tests/run

install: $(BINOUT)/harec
	install -Dm755 $(BINOUT)/harec $(DESTDIR)$(BINDIR)/harec

uninstall:
	rm -- '$(DESTDIR)$(BINDIR)/harec'

.PHONY: clean check install uninstall

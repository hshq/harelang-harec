.POSIX:

all:

include config.mk
include makefiles/$(PLATFORM).mk
include makefiles/tests.mk

all: $(BINOUT)/harec

C_DEFINES = \
	-DVERSION='"'"$(VERSION)"'"' \
	-DDEFAULT_TARGET='"$(DEFAULT_TARGET)"'

headers = include/*.h

harec_objects = \
	src/check.o \
	src/emit.o \
	src/eval.o \
	src/expr.o \
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
src/expr.o: $(headers)
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
	@$(AS) $(ASFLAGS) -o $@ $<

.ssa.s:
	@$(QBE) $(QBEFLAGS) -o $@ $<

.ssa.td:
	@cmp -s $@ $@.tmp 2>/dev/null || mv $@.tmp $@

.ha.ssa:
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $<

clean:
	@rm -rf -- $(HARECACHE) $(BINOUT) $(harec_objects) $(tests)

check: $(BINOUT)/harec
	@echo "\nMOD/ :[HAREC]> .ssa :[QBE]> .s :[AS]> MOD.o"

	@echo "rt\t$(patsubst  %,\n\t%,$(sort $(patsubst rt/%,%,$(rt_ha))))"
	@make $(HARECACHE)/rt.o

	@echo "testmod\t$(patsubst  %,\n\t%,$(patsubst testmod/%,%,$(testmod_ha)))\n"
	@make $(HARECACHE)/testmod.o

	@echo "tests/ID-TEST.ha :[HAREC]> .ssa :[QBE]> .s :[AS]> .o :[LD]> EXE"
	@#echo $(notdir $(tests))
	@make tests
	@$(TDENV) ./tests/run

install: $(BINOUT)/harec
	@#install -Dm755 $(BINOUT)/harec $(DESTDIR)$(BINDIR)/harec
	@install -dm755 $(DESTDIR)$(BINDIR)
	install -m755 $(BINOUT)/harec $(DESTDIR)$(BINDIR)/harec

uninstall:
	rm -- '$(DESTDIR)$(BINDIR)/harec'

.PHONY: clean check install uninstall

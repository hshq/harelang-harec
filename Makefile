.POSIX:

all:

include config.mk
include makefiles/$(PLATFORM).mk
include makefiles/tests.mk

all: $(BINOUT)/harec

C_DEFINES = \
	-DVERSION='"'"$(VERSION)"'"' \
	-DDEFAULT_TARGET='"$(DEFAULT_TARGET)"'

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

.PHONY: clean check install

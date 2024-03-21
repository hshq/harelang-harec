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

# harec_srcs = src/*.c
# harec_objects := $(harec_srcs:.c=.o)
# harec_objects = $(patsubst %.c,%.o,$(harec_srcs))
# harec_objects = $(patsubst %.c,%.o,$(shell ls src/*.c))
harec_objects = $(patsubst %.c,%.o,$(shell ls src/*.c))

$(BINOUT)/harec: $(harec_objects)
	@mkdir -p -- $(BINOUT)
	@printf 'CCLD\t%s\n' '$@'
	@$(CC) $(LDFLAGS) -o $@ $(harec_objects) $(LIBS)

.SUFFIXES:
.SUFFIXES: .ha .ssa .td .c .o .s .scd .1 .5

$(harec_objects): $(headers)

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
	@rm -rf -- $(HARECACHE) $(BINOUT) $(harec_objects) $(tests) tests/ssa-bin.mk

check: $(BINOUT)/harec tests/ssa-bin.mk
	make -f tests/ssa-bin.mk
	@$(TDENV) ./tests/run

install: $(BINOUT)/harec
	@# install -Dm755 $(BINOUT)/harec $(DESTDIR)$(BINDIR)/harec
	@install -dm755 $(DESTDIR)$(BINDIR)
	install -m755 $(BINOUT)/harec $(DESTDIR)$(BINDIR)/harec

uninstall:
	rm -- '$(DESTDIR)$(BINDIR)/harec'

.PHONY: clean check install uninstall

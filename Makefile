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
harec_objects = $(patsubst %.c,$(HARECACHE)/%.o,$(shell ls src/*.c))

$(BINOUT)/harec: $(harec_objects)
	@mkdir -p -- $(BINOUT)
	@printf 'CCLD\t%s\n' '$@'
	@$(CC) $(LDFLAGS) -o $@ $(harec_objects) $(LIBS)

.SUFFIXES:
.SUFFIXES: .ssa .td .c .o .s .scd .1 .5

.PRECIOUS: %.td %.ssa %.s %.o

$(HARECACHE)/%.o: %.c $(headers)
	@mkdir -p -- $(dir $@)
	@printf 'CC\t%-16s --> $$HARECACHE/%s\n' '$<' $(notdir $@)
	@$(CC) -c $(CFLAGS) $(C_DEFINES) -o $@ $(patsubst $(HARECACHE)/%.o,%.c,$@)

# .c.o:
# 	@printf 'CC\t%s\n' '$@'
# 	@$(CC) -c $(CFLAGS) $(C_DEFINES) -o $@ $<

.s.o:
	@$(AS) $(ASFLAGS) -o $@ $<

.ssa.s:
	@$(QBE) $(QBEFLAGS) -o $@.tmp $<
	@mv $@.tmp $@

.ssa.td:
	@cmp -s $@ $@.tmp 2>/dev/null || mv $@.tmp $@

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

TDENV = env HARE_TD_rt=$(HARECACHE)/rt.td HARE_TD_testmod=$(HARECACHE)/testmod.td
test_objects = \
	src/lex.o \
	src/parse.o \
	src/type_store.o \
	src/scope.o \
	src/identifier.o \
	src/util.o \
	src/types.o \
	src/check.o \
	src/utf8.o \
	src/eval.o \
	src/typedef.o \
	src/mod.o

testmod_ha = testmod/measurement.ha testmod/testmod.ha
$(HARECACHE)/testmod.ssa: $(testmod_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ -t $(HARECACHE)/testmod.td.tmp -N testmod $(testmod_ha)

rt_ha = \
	rt/abort.ha \
	rt/compile.ha \
	rt/cstrings.ha \
	rt/ensure.ha \
	rt/itos.ha \
	rt/memcpy.ha \
	rt/memmove.ha \
	rt/memset.ha \
	rt/strcmp.ha \
	rt/+$(PLATFORM)/errno.ha \
	rt/+$(PLATFORM)/syscalls.ha \
	rt/+$(PLATFORM)/start.ha \
	$(_rt_ha)

$(HARECACHE)/rt.ssa: $(rt_ha) $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ -t $(HARECACHE)/rt.td.tmp -N rt $(rt_ha)

rt_s = $(HARECACHE)/rt.s $(_rt_s)
$(HARECACHE)/rt.o: $(rt_s)
	@printf 'AS\t%s\n' '$@'
	@$(AS) $(ASFLAGS) -o $@ $(rt_s)

ha_tests = $(patsubst %.ha,%,$(shell ls tests/*-*.ha))
# ha_tests =
tests = $(patsubst %,$(HARECACHE)/%,$(sort tests/30-reduction $(ha_tests)))


tests/ssa-bin.mk:
	@echo "include Makefile" >> $@;
	@echo all: '$$(tests)' >> $@;
	@echo "\n" >> $@;
	@\
	for T in $(ha_tests); do \
		case "$${T}" in \
			tests/10-binarithms|tests/15-enums|tests/24-imports|tests/34-declarations) \
				TMO='$$(HARECACHE)/testmod.o' \
				TMTD='$$(HARECACHE)/testmod.td' \
				;; \
			*) \
				;; \
		esac; \
		\
		echo '$$(HARECACHE)'/$${T}:  '$$(HARECACHE)'/rt.o '$$(HARECACHE)'/$${T}.o $${TMO} >> $@; \
		echo '	@printf "LD\\t%s\\t\\n" $$@' >> $@; \
		echo '	@$$(LD) $$(LDLINKFLAGS) -T $$(RTSCRIPT) -o $$@ $$^' >> $@; \
		echo '' >> $@; \
		\
		echo '$$(HARECACHE)'/$${T}.ssa: $${T}.ha '$$(HARECACHE)'/rt.td '$$(BINOUT)'/harec $${TMTD} >> $@; \
		echo '	'@mkdir -p -- '$$(HARECACHE)/tests' >> $@; \
		echo '	@printf "HAREC\\t%s\\n" $$@' >> $@; \
		echo '	@$$(TDENV) $$(BINOUT)/harec $$(HARECFLAGS) -o $$@ $$<' >> $@; \
		echo "\n" >> $@; \
	done


$(HARECACHE)/tests/30-reduction.o: tests/30-reduction.c
	@mkdir -p -- $(HARECACHE)/tests
	@printf 'CC\t%s\n' '$@'
	@$(CC) -c $(CFLAGS) $(C_DEFINES) -o $@ $<

$(HARECACHE)/tests/30-reduction: $(HARECACHE)/tests/30-reduction.o $(test_objects)
	@printf 'CCLD\t%s\n' '$@'
	@$(CC) $(LDFLAGS) $(LIBS) -o $@ $^

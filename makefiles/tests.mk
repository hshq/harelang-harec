TDENV = env HARE_TD_rt=$(HARECACHE)/rt.td HARE_TD_testmod=$(HARECACHE)/testmod.td
test_objects = $(patsubst %,$(HARECACHE)/%,\
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
	src/expr.o \
	src/typedef.o \
	src/mod.o)

testmod_ha = testmod/measurement.ha testmod/testmod.ha
$(HARECACHE)/testmod.ssa: $(testmod_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
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
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ -t $(HARECACHE)/rt.td.tmp -N rt $(rt_ha)

rt_s = $(HARECACHE)/rt.s $(_rt_s)
$(HARECACHE)/rt.o: $(rt_s)
	@$(AS) $(ASFLAGS) -o $@ $(rt_s)

ha_tests = $(patsubst %.ha,%,$(shell ls tests/*-*.ha))
# ha_tests =
tests = $(patsubst %,$(HARECACHE)/%,$(sort tests/30-reduction $(ha_tests)))


tests: $(tests)


$(HARECACHE)/tests/%: $(HARECACHE)/rt.o $(HARECACHE)/tests/%.o
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $^

$(HARECACHE)/tests/%.ssa: tests/%.ha $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)/tests
	@echo $(notdir $<)
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $<


$(HARECACHE)/tests/10-binarithms: 	$(HARECACHE)/testmod.o
$(HARECACHE)/tests/15-enums: 		$(HARECACHE)/testmod.o
$(HARECACHE)/tests/24-imports: 		$(HARECACHE)/testmod.o
$(HARECACHE)/tests/34-declarations: $(HARECACHE)/testmod.o


$(HARECACHE)/tests/10-binarithms.ssa: 	$(HARECACHE)/testmod.td
$(HARECACHE)/tests/15-enums.ssa: 		$(HARECACHE)/testmod.td
$(HARECACHE)/tests/24-imports.ssa: 		$(HARECACHE)/testmod.td
$(HARECACHE)/tests/34-declarations.ssa: $(HARECACHE)/testmod.td


$(HARECACHE)/tests/30-reduction.o: tests/30-reduction.c
	@mkdir -p -- $(HARECACHE)/tests
	@printf '%-20s :[CC]> .o' $(notdir $<)
	@$(CC) -c $(CFLAGS) $(C_DEFINES) -o $@ $<

$(HARECACHE)/tests/30-reduction: $(HARECACHE)/tests/30-reduction.o $(test_objects)
	@printf ' :[CCLD]> %s\n' $(notdir $@)
	@$(CC) $(LDFLAGS) $(LIBS) -o $@ $^

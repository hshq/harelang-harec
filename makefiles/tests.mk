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
	src/expr.o \
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

tests = \
	tests/00-literals \
	tests/01-arrays \
	tests/02-integers \
	tests/03-pointers \
	tests/04-strings \
	tests/05-implicit-casts \
	tests/06-structs \
	tests/07-aliases \
	tests/08-slices \
	tests/09-funcs \
	tests/10-binarithms \
	tests/11-globals \
	tests/12-loops \
	tests/13-tagged \
	tests/14-switch \
	tests/15-enums \
	tests/16-defer \
	tests/17-alloc \
	tests/18-match \
	tests/19-append \
	tests/20-if \
	tests/21-tuples \
	tests/22-delete \
	tests/23-errors \
	tests/24-imports \
	tests/25-promotion \
	tests/26-regression \
	tests/27-rt \
	tests/28-insert \
	tests/29-unarithm \
	tests/30-reduction \
	tests/31-postfix \
	tests/32-copy \
	tests/33-yield \
	tests/34-declarations \
	tests/35-floats \
	tests/36-defines


tests/00-literals: $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_00_literals.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_00_literals.o

tests_00_literals_ha = tests/00-literals.ha
$(HARECACHE)/tests_00_literals.ssa: $(tests_00_literals_ha) $(HARECACHE)/rt.td $(HARECACHE)/testmod.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_00_literals_ha)


tests/01-arrays: $(HARECACHE)/rt.o $(HARECACHE)/tests_01_arrays.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_01_arrays.o

tests_01_arrays_ha = tests/01-arrays.ha
$(HARECACHE)/tests_01_arrays.ssa: $(tests_01_arrays_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_01_arrays_ha)


tests/02-integers: $(HARECACHE)/rt.o $(HARECACHE)/tests_02_integers.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_02_integers.o

tests_02_integers_ha = tests/02-integers.ha
$(HARECACHE)/tests_02_integers.ssa: $(tests_02_integers_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_02_integers_ha)


tests/03-pointers: $(HARECACHE)/rt.o $(HARECACHE)/tests_03_pointers.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_03_pointers.o

tests_03_pointers_ha = tests/03-pointers.ha
$(HARECACHE)/tests_03_pointers.ssa: $(tests_03_pointers_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_03_pointers_ha)


tests/04-strings: $(HARECACHE)/rt.o $(HARECACHE)/tests_04_strings.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_04_strings.o

tests_04_strings_ha = tests/04-strings.ha
$(HARECACHE)/tests_04_strings.ssa: $(tests_04_strings_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_04_strings_ha)


tests/05-implicit-casts: $(HARECACHE)/rt.o $(HARECACHE)/tests_05_implicit_casts.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_05_implicit_casts.o

tests_05_implicit_casts_ha = tests/05-implicit-casts.ha
$(HARECACHE)/tests_05_implicit_casts.ssa: $(tests_05_implicit_casts_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_05_implicit_casts_ha)


tests/06-structs: $(HARECACHE)/rt.o $(HARECACHE)/tests_06_structs.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_06_structs.o

tests_06_structs_ha = tests/06-structs.ha
$(HARECACHE)/tests_06_structs.ssa: $(tests_06_structs_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_06_structs_ha)


tests/07-aliases: $(HARECACHE)/rt.o $(HARECACHE)/tests_07_aliases.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_07_aliases.o

tests_07_aliases_ha = tests/07-aliases.ha
$(HARECACHE)/tests_07_aliases.ssa: $(tests_07_aliases_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_07_aliases_ha)


tests/08-slices: $(HARECACHE)/rt.o $(HARECACHE)/tests_08_slices.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_08_slices.o

tests_08_slices_ha = tests/08-slices.ha
$(HARECACHE)/tests_08_slices.ssa: $(tests_08_slices_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_08_slices_ha)


tests/09-funcs: $(HARECACHE)/rt.o $(HARECACHE)/tests_09_funcs.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_09_funcs.o

tests_09_funcs_ha = tests/09-funcs.ha
$(HARECACHE)/tests_09_funcs.ssa: $(tests_09_funcs_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_09_funcs_ha)


tests/10-binarithms: $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_10_binarithms.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_10_binarithms.o

tests_10_binarithms_ha = tests/10-binarithms.ha
$(HARECACHE)/tests_10_binarithms.ssa: $(tests_10_binarithms_ha) $(HARECACHE)/rt.td $(HARECACHE)/testmod.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_10_binarithms_ha)


tests/11-globals: $(HARECACHE)/rt.o $(HARECACHE)/tests_11_globals.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_11_globals.o

tests_11_globals_ha = tests/11-globals.ha
$(HARECACHE)/tests_11_globals.ssa: $(tests_11_globals_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_11_globals_ha)


tests/12-loops: $(HARECACHE)/rt.o $(HARECACHE)/tests_12_loops.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_12_loops.o

tests_12_loops_ha = tests/12-loops.ha
$(HARECACHE)/tests_12_loops.ssa: $(tests_12_loops_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_12_loops_ha)


tests/13-tagged: $(HARECACHE)/rt.o $(HARECACHE)/tests_13_tagged.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_13_tagged.o

tests_13_tagged_ha = tests/13-tagged.ha
$(HARECACHE)/tests_13_tagged.ssa: $(tests_13_tagged_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_13_tagged_ha)


tests/14-switch: $(HARECACHE)/rt.o $(HARECACHE)/tests_14_switch.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_14_switch.o

tests_14_switch_ha = tests/14-switch.ha
$(HARECACHE)/tests_14_switch.ssa: $(tests_14_switch_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_14_switch_ha)


tests/15-enums: $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_15_enums.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_15_enums.o

tests_15_enums_ha = tests/15-enums.ha
$(HARECACHE)/tests_15_enums.ssa: $(tests_15_enums_ha) $(HARECACHE)/rt.td $(HARECACHE)/testmod.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_15_enums_ha)


tests/16-defer: $(HARECACHE)/rt.o $(HARECACHE)/tests_16_defer.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_16_defer.o

tests_16_defer_ha = tests/16-defer.ha
$(HARECACHE)/tests_16_defer.ssa: $(tests_16_defer_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_16_defer_ha)


tests/17-alloc: $(HARECACHE)/rt.o $(HARECACHE)/tests_17_alloc.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_17_alloc.o

tests_17_alloc_ha = tests/17-alloc.ha
$(HARECACHE)/tests_17_alloc.ssa: $(tests_17_alloc_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_17_alloc_ha)


tests/18-match: $(HARECACHE)/rt.o $(HARECACHE)/tests_18_match.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_18_match.o

tests_18_match_ha = tests/18-match.ha
$(HARECACHE)/tests_18_match.ssa: $(tests_18_match_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_18_match_ha)


tests/19-append: $(HARECACHE)/rt.o $(HARECACHE)/tests_19_append.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_19_append.o

tests_19_append_ha = tests/19-append.ha
$(HARECACHE)/tests_19_append.ssa: $(tests_19_append_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_19_append_ha)


tests/20-if: $(HARECACHE)/rt.o $(HARECACHE)/tests_20_if.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_20_if.o

tests_20_if_ha = tests/20-if.ha
$(HARECACHE)/tests_20_if.ssa: $(tests_20_if_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_20_if_ha)


tests/21-tuples: $(HARECACHE)/rt.o $(HARECACHE)/tests_21_tuples.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_21_tuples.o

tests_21_tuples_ha = tests/21-tuples.ha
$(HARECACHE)/tests_21_tuples.ssa: $(tests_21_tuples_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_21_tuples_ha)


tests/22-delete: $(HARECACHE)/rt.o $(HARECACHE)/tests_22_delete.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_22_delete.o

tests_22_delete_ha = tests/22-delete.ha
$(HARECACHE)/tests_22_delete.ssa: $(tests_22_delete_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_22_delete_ha)


tests/23-errors: $(HARECACHE)/rt.o $(HARECACHE)/tests_23_errors.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_23_errors.o

tests_23_errors_ha = tests/23-errors.ha
$(HARECACHE)/tests_23_errors.ssa: $(tests_23_errors_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_23_errors_ha)


tests/24-imports: $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_24_imports.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_24_imports.o

tests_24_imports_ha = tests/24-imports.ha
$(HARECACHE)/tests_24_imports.ssa: $(tests_24_imports_ha) $(HARECACHE)/rt.td $(HARECACHE)/testmod.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_24_imports_ha)


tests/25-promotion: $(HARECACHE)/rt.o $(HARECACHE)/tests_25_promotion.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_25_promotion.o

tests_25_promotion_ha = tests/25-promotion.ha
$(HARECACHE)/tests_25_promotion.ssa: $(tests_25_promotion_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_25_promotion_ha)


tests/26-regression: $(HARECACHE)/rt.o $(HARECACHE)/tests_26_regression.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_26_regression.o

tests_26_regression_ha = tests/26-regression.ha
$(HARECACHE)/tests_26_regression.ssa: $(tests_26_regression_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_26_regression_ha)


tests/27-rt: $(HARECACHE)/rt.o $(HARECACHE)/tests_27_rt.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_27_rt.o

tests_27_rt_ha = tests/27-rt.ha
$(HARECACHE)/tests_27_rt.ssa: $(tests_27_rt_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_27_rt_ha)


tests/28-insert: $(HARECACHE)/rt.o $(HARECACHE)/tests_28_insert.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_28_insert.o

tests_28_insert_ha = tests/28-insert.ha
$(HARECACHE)/tests_28_insert.ssa: $(tests_28_insert_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_28_insert_ha)


tests/29-unarithm: $(HARECACHE)/rt.o $(HARECACHE)/tests_29_unarithm.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_29_unarithm.o

tests_29_unarithm_ha = tests/29-unarithm.ha
$(HARECACHE)/tests_29_unarithm.ssa: $(tests_29_unarithm_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_29_unarithm_ha)


tests/30-reduction: tests/30-reduction.o $(test_objects)
	@printf 'CCLD\t%s\n' '$@'
	@$(CC) $(LDFLAGS) $(LIBS) -o $@ tests/30-reduction.o $(test_objects)


tests/31-postfix: $(HARECACHE)/rt.o $(HARECACHE)/tests_31_postfix.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_31_postfix.o

tests_31_postfix_ha = tests/31-postfix.ha
$(HARECACHE)/tests_31_postfix.ssa: $(tests_31_postfix_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_31_postfix_ha)


tests/32-copy: $(HARECACHE)/rt.o $(HARECACHE)/tests_32_copy.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_32_copy.o

tests_32_copy_ha = tests/32-copy.ha
$(HARECACHE)/tests_32_copy.ssa: $(tests_32_copy_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_32_copy_ha)


tests/33-yield: $(HARECACHE)/rt.o $(HARECACHE)/tests_33_yield.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_33_yield.o

tests_33_yield_ha = tests/33-yield.ha
$(HARECACHE)/tests_33_yield.ssa: $(tests_33_yield_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_33_yield_ha)


tests/34-declarations: $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_34_declarations.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/testmod.o $(HARECACHE)/tests_34_declarations.o

tests_34_declarations_ha = tests/34-declarations.ha
$(HARECACHE)/tests_34_declarations.ssa: $(tests_34_declarations_ha) $(HARECACHE)/rt.td $(HARECACHE)/testmod.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_34_declarations_ha)


tests/35-floats: $(HARECACHE)/rt.o $(HARECACHE)/tests_35_floats.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_35_floats.o

tests_35_floats_ha = tests/35-floats.ha
$(HARECACHE)/tests_35_floats.ssa: $(tests_35_floats_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_35_floats_ha)


tests/36-defines: $(HARECACHE)/rt.o $(HARECACHE)/tests_36_defines.o
	@printf 'LD\t%s\t\n' '$@'
	@$(LD) $(LDLINKFLAGS) -T $(RTSCRIPT) -o $@ $(HARECACHE)/rt.o $(HARECACHE)/tests_36_defines.o

tests_36_defines_ha = tests/36-defines.ha
$(HARECACHE)/tests_36_defines.ssa: $(tests_36_defines_ha) $(HARECACHE)/rt.td $(BINOUT)/harec
	@mkdir -p -- $(HARECACHE)
	@printf 'HAREC\t%s\n' '$@'
	@$(TDENV) $(BINOUT)/harec $(HARECFLAGS) -o $@ $(tests_36_defines_ha)

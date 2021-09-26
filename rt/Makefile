libhart_srcs+=\
	rt/abort.ha \
	rt/cstrings.ha \
	rt/compile.ha \
	rt/ensure.ha \
	rt/malloc.ha \
	rt/memcpy.ha \
	rt/memmove.ha \
	rt/memset.ha \
	rt/rtmain.ha \
	rt/strcmp.ha \
	rt/types.ha

libhart.a: harec $(libhart_srcs) $(libhart_objs) $(rtstart) types/types.o
	@printf 'HAREC\t$@\n'
	@mkdir -p $(HARECACHE)/rt
	@./harec -Nrt -t$(HARECACHE)/rt/rt.td -o $@.ssa $(libhart_srcs)
	@$(QBE) -o $@.s $@.ssa
	@$(AS) -g -o $@.o $@.s
	@$(AR) -csr $@ $@.o $(libhart_objs) types/types.o
	@rm $@.o $@.s $@.ssa

clean-rt:
	@rm -f libhart.a $(libhart_objs) $(rtstart)

.PHONY: rt clean-rt

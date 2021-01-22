libhart_srcs+=\
	rt/abort.ha \
	rt/compile.ha \
	rt/malloc.ha \
	rt/memcpy.ha \
	rt/rtmain.ha

libhart.a: harec $(libhart_srcs) $(libhart_objs) $(rtstart)
	@printf 'HAREC\t$@\n'
	@mkdir -p $(HARECACHE)/rt
	@./harec -Nrt -t$(HARECACHE)/rt/rt.td -o $@.ssa $(libhart_srcs)
	@qbe -o $@.s $@.ssa
	@$(AS) -o $@.o $@.s
	@$(AR) -csr $@ $@.o $(libhart_objs)
	@rm $@.o $@.s $@.ssa

clean-rt:
	@rm -f libhart.a $(libhart_objs) $(rtstart)

.PHONY: rt clean-rt

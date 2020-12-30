libhart_srcs=\
	rt/assert.ha \
	rt/malloc.ha \
	rt/memcmp.ha \
	rt/memcpy.ha \
	rt/memset.ha

libhart.a: $(libhart_srcs)
	@printf 'HAREC\t$@\n'
	@./harec -N rt -o $@.ssa $(libhart_srcs)
	@qbe -o $@.s $@.ssa
	@$(AS) -o $@.o $@.s
	@$(AR) -csr $@ $@.o
	@rm $@.o $@.s $@.ssa

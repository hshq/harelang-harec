libhart_srcs+=\
	rt/abort.ha \
	rt/compile.ha \
	rt/rtmain.ha

libhart.a: harec $(libhart_srcs) $(libhart_objs)
	@printf 'HAREC\t$@\n'
	@./harec -N rt -o $@.ssa $(libhart_srcs)
	@qbe -o $@.s $@.ssa
	@$(AS) -o $@.o $@.s
	@$(AR) -csr $@ $@.o $(libhart_objs)
	@rm $@.o $@.s $@.ssa

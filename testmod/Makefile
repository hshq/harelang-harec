testmod_srcs+=\
	testmod/testmod.ha

testmod.a: harec $(testmod_srcs)
	@printf 'HAREC\t$@\n'
	@mkdir -p $(HARECACHE)/testmod
	@./harec -Ntestmod  -t$(HARECACHE)/testmod/testmod.td -o $@.ssa $(testmod_srcs)
	@$(QBE) -o $@.s $@.ssa
	@$(AS) -o $@.o $@.s
	@$(AR) -csr $@ $@.o $(testmod_objs)
	@rm $@.o $@.s $@.ssa

clean-testmod:
	@rm -f testmod.a

.PHONY: testmod clean-testmod

testmod_srcs+=\
	testmod/testmod.ha

hare_env+=HARE_TD_testmod=$(HARECACHE)/testmod/testmod.td

testmod.a: harec $(testmod_srcs)
	@printf 'HAREC\t$@\n'
	@mkdir -p $(HARECACHE)/testmod
	@$(hare_env) ./harec -Ntestmod  -t$(HARECACHE)/testmod/testmod.td -o $@.ssa $(testmod_srcs)
	@$(QBE) -o $@.s $@.ssa
	@$(AS) -g -o $@.o $@.s
	@$(AR) -csr $@ $@.o $(testmod_objs)
	@rm $@.o $@.s $@.ssa

clean-testmod:
	@rm -f testmod.a

.PHONY: testmod clean-testmod

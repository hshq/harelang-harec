types_srcs+=\
	types/reflect.ha

types/types.o: harec $(types_srcs)
	@printf 'HAREC\t$@\n'
	@mkdir -p $(HARECACHE)/types
	@./harec -Ntypes -t$(HARECACHE)/types/types.td -o $@.ssa $(types_srcs)
	@$(QBE) -o $@.s $@.ssa
	@$(AS) -g -o $@ $@.s
	@rm $@.s $@.ssa

clean-types:
	@rm -f types/types.o

.PHONY: types clean-types

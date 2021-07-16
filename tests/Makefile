check:
	@./tests/run

tests/rt.o: tests/rt.ha
	@printf 'HARECC\t%s\t$@\n' "tests/rt"
	@HARECACHE=\$(HARECACHE) ./harec -o tests/rt.ssa tests/rt.ha
	@\$(QBE) -o tests/rt.s tests/rt.ssa
	@\$(CC) -c -o tests/rt.o tests/rt.s

.PHONY: check clean-tests

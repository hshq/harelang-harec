.text
.global _start
_start:
	xor %rbp, %rbp
	movq %rsp, %rdi
	andq $-16, %rsp
	call _rt.start_ha


.private_extern ___init_array_start
___init_array_start = section$start$__DATA$.init_array

.private_extern ___init_array_end
___init_array_end = section$end$__DATA$.init_array

.private_extern ___fini_array_start
___fini_array_start = section$start$__DATA$.fini_array

.private_extern ___fini_array_end
___fini_array_end = section$end$__DATA$.fini_array

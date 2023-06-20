.text
.global _start
_start:
	xor %rbp, %rbp
	andq $-16, %rsp
	leaq -8(%rsi), %rdi
	call _rt.start_ha


.private_extern ___init_array_start
___init_array_start = section$start$__DATA$.init_array

.private_extern ___init_array_end
___init_array_end = section$end$__DATA$.init_array

.private_extern ___fini_array_start
___fini_array_start = section$start$__DATA$.fini_array

.private_extern ___fini_array_end
___fini_array_end = section$end$__DATA$.fini_array

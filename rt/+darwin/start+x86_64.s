.text
.global _start
_start:
	xor %rbp, %rbp
	andq $-16, %rsp
	leaq -8(%rsi), %rdi
	call _rt.start_ha

.include "start-libc.s"
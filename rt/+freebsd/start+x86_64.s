.text
.global _start
_start:
	xor %rbp, %rbp
	andq $-16, %rsp
	call rt.start_ha

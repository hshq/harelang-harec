	.section ".note.openbsd.ident", "a"
	.p2align 2
	.long	0x8
	.long	0x4
	.long	0x1
	.asciz	"OpenBSD"
	.long	0x0
	.previous

.text
.global _start
_start:
	xor %rbp, %rbp
	movq %rsp, %rdi
	andq $-16, %rsp
	call rt.start_ha

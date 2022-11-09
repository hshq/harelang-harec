# 0x2000000: #define S(n) ((2 << 24) | (~(0xff << 24) & (n)))

.text # .section .text.rt.syscall0
.global _rt.syscall0
_rt.syscall0:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	syscall
	ret

.text # .section .text.rt.syscall1
.global _rt.syscall1
_rt.syscall1:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	movq %rsi, %rdi
	syscall
	ret

.text # .section .text.rt.syscall2
.global _rt.syscall2
_rt.syscall2:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	movq %rsi, %rdi
	movq %rdx, %rsi
	syscall
	ret

.text # .section .text.rt.syscall3
.global _rt.syscall3
_rt.syscall3:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	movq %rsi, %rdi
	movq %rdx, %rsi
	movq %rcx, %rdx
	syscall
	ret

.text # .section .text.rt.syscall4
.global _rt.syscall4
_rt.syscall4:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	movq %r8, %r10
	movq %rsi, %rdi
	movq %rdx, %rsi
	movq %rcx, %rdx
	syscall
	ret

.text # .section .text.rt.syscall5
.global _rt.syscall5
_rt.syscall5:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	movq %r8, %r10
	movq %rsi, %rdi
	movq %r9, %r8
	movq %rdx, %rsi
	movq %rcx, %rdx
	syscall
	ret

.text # .section .text.rt.syscall6
.global _rt.syscall6
_rt.syscall6:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	movq %r8, %r10
	movq %rsi, %rdi
	movq %r9, %r8
	movq %rdx, %rsi
	movq 8(%rsp), %r9
	movq %rcx, %rdx
	syscall
	# movq %r11, %rdx
	# movq %rax, %rcx
	# negq %rcx
	# testb $1, %dl
	# cmoveq %rax, %rcx
	# movq %rcx, %rax
	ret


.text # .section .text.rt.sys_pipe
.global _rt.sys_pipe
_rt.sys_pipe:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	# movq %rsi, %rdi
	syscall
	movl %eax, (%rsi)
	movl %edx, 4(%rsi)
	movq $0, %rax
	ret

.text # .section .text.rt.sys_fork
.global _rt.sys_fork
_rt.sys_fork:
	# movq %rdi, %rax
	leaq 0x2000000(%rdi), %rax
	syscall
	xorq %rdi, %rdi
	cmpq $1, %rdx
	cmoveq %rdi, %rax
	ret

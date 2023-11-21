.text ; .section .text.rt.syscall0
.global _rt.syscall0
_rt.syscall0:
	mov		x16, x0
	svc		#0x80
	ret

.text ; .section .text.rt.syscall1
.global _rt.syscall1
_rt.syscall1:
	mov		x16, x0
	mov 	x0, x1
	svc		#0x80
	ret

.text ; .section .text.rt.syscall2
.global _rt.syscall2
_rt.syscall2:
	mov		x16, x0
	mov 	x0, x1
	mov 	x1, x2
	svc		#0x80
	ret

.text ; .section .text.rt.syscall3
.global _rt.syscall3
_rt.syscall3:
	mov		x16, x0
	mov 	x0, x1
	mov 	x1, x2
	mov 	x2, x3
	svc		#0x80
	ret

.text ; .section .text.rt.syscall4
.global _rt.syscall4
_rt.syscall4:
	mov		x16, x0
	mov 	x0, x1
	mov 	x1, x2
	mov 	x2, x3
	mov 	x3, x4
	svc		#0x80
	ret

.text ; .section .text.rt.syscall5
.global _rt.syscall5
_rt.syscall5:
	mov		x16, x0
	mov 	x0, x1
	mov 	x1, x2
	mov 	x2, x3
	mov 	x3, x4
	mov 	x4, x5
	svc		#0x80
	ret

.text ; .section .text.rt.syscall6
.global _rt.syscall6
_rt.syscall6:
	mov		x16, x0
	mov 	x0, x1
	mov 	x1, x2
	mov 	x2, x3
	mov 	x3, x4
	mov 	x4, x5
	mov 	x5, x6
	svc		#0x80
	ret


.text ; .section .text.rt.sys_pipe
.global _rt.sys_pipe
_rt.sys_pipe:
	mov		x9, x1			; Stash FD array
	mov		x16, x0
	svc		#0x80
	stp 	w0, w1, [x9]	; Save results
	mov 	x0, #0			; Success
	ret

.text ; .section .text.rt.sys_fork
.global _rt.sys_fork
_rt.sys_fork:
	// ARM moves a 1 in to r1 here, but I can't see why.
	mov 	x16, x0						// Syscall code
	svc 	#0x80						// Trap to kernel
	b.cs	Lbotch						// Carry bit indicates failure
	cbz		x1, Lparent					// x1 == 0 indicates that we are the parent

	// Child
	mov		x0, #0
	ret

Lbotch:
	mov		x0, #-1						// Return value is -1
Lparent:
	ret									// Return

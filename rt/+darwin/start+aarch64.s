.text
.global _start
_start:
	mov 	x29, #0
	mov 	x30, #0
	mov 	x0, sp
	add 	sp, x0, #-16
	b 		_rt.start_ha

; .include "rt/+darwin/@func-libc.s"
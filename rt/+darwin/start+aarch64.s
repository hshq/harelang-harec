.text
.global _start
_start:
	mov 	x29, #0
	mov 	x30, #0
	mov 	x0, sp
	add 	sp, x0, #-16
	b 		_rt.start_ha


.private_extern ___init_array_start
___init_array_start = section$start$__DATA$.init_array

.private_extern ___init_array_end
___init_array_end = section$end$__DATA$.init_array

.private_extern ___fini_array_start
___fini_array_start = section$start$__DATA$.fini_array

.private_extern ___fini_array_end
___fini_array_end = section$end$__DATA$.fini_array

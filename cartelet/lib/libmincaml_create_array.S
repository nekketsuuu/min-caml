# create_array
min_caml_create_array:
	add	%r2 %r0 %r28
	add	%r8 %r0 %r16
	add	%r9 %r0 %r28
	addi	%r1 %r0 min_caml_create_array_loop
min_caml_create_array_loop:
	st	0(%r9) %r17
	addi	%r8 %r8 $-1
	addi	%r9 %r9 $1
	beq	%r8 %r0 min_caml_create_array_exit
	jr	%r1
min_caml_create_array_exit:
	jr	%r31
# create_float_array
min_caml_create_float_array:
	add	%r2 %r0 %r28
	add	%r8 %r0 %r16
	add	%r9 %r0 %r28
	addi	%r1 %r0 min_caml_create_array_loop
min_caml_create_array_loop:
	fst	0(%r9) %f0
	addi	%r8 %r8 $-1
	addi	%r9 %r9 $1
	beq	%r8 %r0 min_caml_create_array_exit
	jr	%r1
min_caml_create_array_exit:
	jr	%r31

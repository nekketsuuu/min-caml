# print_newline
min_caml_print_newline:
	# LF is 0x0a = 10
	addi	%r1 %r0 $10
	send8	%r1
	jr	%r31
# print_char
min_caml_print_char:
	send8	%r16
	jr	%r31
# read_int
min_caml_read_int:
	addi	%r1 %r0 $8
	recv8	%r2
	sll	%r2 %r2 %r1
	recv8	%r2
	sll	%r2 %r2 %r1
	recv8	%r2
	sll	%r2 %r2 %r1
	recv8	%r2
	jr	%r31
# read_float
min_caml_read_float:
	addi	%r1 %r0 $8
	recv8	%r2
	sll	%r2 %r2 %r1
	recv8	%r2
	sll	%r2 %r2 %r1
	recv8	%r2
	sll	%r2 %r2 %r1
	recv8	%r2
	st	0(%r29) %r2
	fld	0(%r29) %f0
	jr	%r31
# create_array
min_caml_create_array:
	add	%r2 %r0 %r28
	add	%r18 %r0 %r28
	addi	%r1 %r0 min_caml_create_array_loop
min_caml_create_array_loop:
	st	0(%r18) %r17
	addi	%r16 %r16 $-1
	addi	%r18 %r18 $1
	beq	%r16 %r0 min_caml_create_array_exit
	jr	%r1
min_caml_create_array_exit:
	jr	%r31
# create_float
min_caml_create_float_array:
	add	%r2 %r0 %r28
	add	%r18 %r0 %r28
	addi	%r1 %r0 min_caml_create_array_loop
min_caml_create_array_loop:
	fst	0(%r18) %f0
	addi	%r16 %r16 $-1
	addi	%r18 %r18 $1
	beq	%r16 %r0 min_caml_create_array_exit
	jr	%r1
min_caml_create_array_exit:
	jr	%r31
min_caml_sqrt:
	sqrtsd	%xmm0, %xmm0
	ret
.globl min_caml_floor
min_caml_floor:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(floor)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_int_of_float
min_caml_int_of_float:
.globl min_caml_truncate
min_caml_truncate:
	cvttsd2si	%xmm0, %eax
	ret
.globl min_caml_float_of_int
min_caml_float_of_int:
	cvtsi2sd	%eax, %xmm0
	ret
.globl min_caml_cos
min_caml_cos:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(cos)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_sin
min_caml_sin:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(sin)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_atan
min_caml_atan:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(atan)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.data
float_0:
	.long	0x0
float_1:
	.long	0x3f800000
.globl min_caml_fnegd
min_caml_fnegd:
	.long	0x80000000

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
# create_float
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
# fsqrt
min_caml_sqrt:
	# 後回し
	sqrtsd	%xmm0, %xmm0
	ret
# ffloor
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
# int_of_float (a.k.a. truncate)
min_caml_int_of_float:
min_caml_truncate:
	cvttsd2si	%xmm0, %eax
	ret
# float_of_int
min_caml_float_of_int:
	cvtsi2sd	%eax, %xmm0
	ret
# cos
min_caml_cos:
	# 定義域を[0, 2pi)にする
	# r8: FLAG
	# f0: x, f1: pi, f2: 0.5, f3: temp(pi/2), f4: temp
	fabs	%f0 %f0
	addi	%r29 %r29 $-1
	st	0(%r29) %r31
	jal	min_caml_reduction_2pi
	ld	0(%r29) %r31
	addi	%r29 %r29 $1
	addi	%r8 %r0 $0
	# x >= piならx := x - pi, FLAG reverse
	addi	%r9 %r0 min_caml_pi
	fld	0(%r9) %f1
	fslt	%f0 %f1
	bclt	min_caml_cos_2
	fneg	%f3 %f1
	fadd	%f0 %f0 %f3
	addi	%r8 %r8 $1
	addi	%r9 %r0 $1
	sll	%r8 %r8 %r9
min_caml_cos_2:
	# x >= pi/2ならx := pi - x, FLAG reverse
	addi	%r9 %r0 min_caml_half
	fld	0(%r9) %f2
	fmul	%f3 %f1 %f2
	fslt	%f0 %f3
	bclt	min_caml_cos_3
	fneg	%f4 %f0
	fadd	%f0 %f1 %f4
	addi	%r8 %r8 $1
	addi	%r9 %r0 $1
	sll	%r8 %r8 %r9
min_caml_cos_3:
	# x <= pi/4ならkernel_cos, そうでないならx := pi/2 - x, kernel_sinする
	fmul	%f4 %f3 %f2
	fslt	%f4 %f0
	bclf	min_caml_kernel_cos
	fneg	%f4 %f0
	fadd	%f0 %f3 %f4
	jr	min_caml_kernal_sin
min_caml_kernel_cos:
	# Tayler展開で計算する
	# r8: flag
	# f0: answer, f1: x^2, f2: x^4, f3: const or product
	fmul	%f1 %f0 %f0
	addi	%r9 %r0 min_caml_float_1
	fld	0(%r9) %f0
	addi	%r9 %r0 min_caml_kernel_cos_1
	fld	0(%r9) %f3
	fmul	%f3 %f3 %f1
	fadd	%f0 %f0 %f3
	fmul	%f2 %f1 %f1
	addi	%r9 %r0 min_caml_kernel_cos_2
	fld	0(%r9) %f3
	fmul	%f3 %f3 %f2
	fadd	%f0 %f0 %f3
	fmul	%f2 %f1 %f2
	addi	%r9 %r0 min_caml_kernel_cos_3
	fld	0(%r9) %f3
	fmul	%f3 %f3 %f2
	fadd	%f0 %f0 %f3
	beq	%r8 %r0 min_caml_kernel_cos_positive
	fabs	%f0 %f0
	fneg	%f0 %f0
	jr	%r31
min_caml_kernel_cos_positive:
	fabs	%f0 %f0
	jr	%r31
# sin
min_caml_sin:

# cos & sin
min_caml_reduction_2pi:
	# f0を[0, 2pi)にする
	# f1: 2*pi, f2: 0.5, f3: p
	addi	%r9 %r0 min_caml_pi
	fld	0(%r9) %f1
	addi	%r9 %r0 min_caml_half
	fld	0(%r9) %f1
	fadd	%f0 %f1 %f1
	fmov	%f3 %f0
min_caml_reduction_2pi_cont:
	fslt	%f0 %f1
	bclt	min_caml_reduction_2pi_exit
	fslt	%f0 %f3
	bclt	min_caml_reduction_2pi_after_if
	fneg	%f4 %f3
	fadd	%f0 %f4
min_caml_reduction_2pi_after_if
	fmul	%f3 %f3 %f2
min_caml_reduction_2pi_exit:
	jr	%r31
# atan
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

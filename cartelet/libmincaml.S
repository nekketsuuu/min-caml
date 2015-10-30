# print_newline
min_caml_print_newline:
	# LF is 0x0a = 10
	addi	%r1 %r0 $10
	send8	%r1
	jr	%r31
# print_char (print_byte)
min_caml_print_byte:
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
	st	-1(%r29) %r2
	fld	-1(%r29) %f0
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
	# 後回し
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
	jr	%r31
# float_of_int
min_caml_float_of_int:
	addi	%r8 %r0 $838860
	slt	%r9 %r16 %r8
	beq	%r9 %r0 min_caml_float_of_int_big
	# |x| < 838860
	# 838860を足して838860.0fを引く
	# r8: const or addr, r9: temp, r16: x
	# f0: answer
	add	%r9 %r16 %r8
	st	-1(%r29) %r9
	fld	-1(%r29) %f0
	addi	%r8 %r0 min_caml_float_of_int_c1
	fld	0(%r8) %f1
	addi	%f0 %f0 %f1
	jr	%r31
min_caml_float_of_int_big:
	# |x| >= 838860
	# x = m*838860 + nとしてitof(m)*838860+itof(n)を求める
	# r8: x or n, r9: m, r10: 838860, r11: -838860, r12: temp, r16: x
	# f0: answer
	add	%r8 %r0 %r16
	add	%r9 %r0 %r0
	addi	%r10 %r0 $838860
	addi	%r11 %r0 $-838860
min_caml_float_of_int_big_loop:
	addi	%r9 %r9 $1
	add	%r8 %r8 %r11
	slt	%r12 %r10 %r8
	beq	%r12 %r0 min_caml_float_of_int_big_loop
	st	-1(%r29) %r8
	st	-2(%r29) %r9
	st	-3(%r29) %r16
	addi	%r29 %r29 $-4
	st	0(%r29) %r31
	add	%r16 %r0 %r9
	jal	min_caml_float_of_int
	ld	0(%r29) %r31
	addi	%r29 %r29 $4
	fst	-4(%r29) %f0
	addi	%r29 %r29 $-5
	st	0(%r29) %r31
	addi	%r16 %r0 $838860
	jal	min_caml_float_of_int
	ld	0(%r29) %r31
	addi	%r29 %r29 $5
	fld	-4(%r29) %f1
	ld	-3(%r29) %r16
	ld	-2(%r29) %r9
	fmov	%f2 %f0
	addi	%r9 %r9 $-1
	addi	%r8 %r0 min_caml_float_of_int_sum_m_loop
min_caml_float_of_int_sum_m_loop:
	beq	%r9 %r0 min_caml_float_of_int_sum_m_exit
	fadd	%f2 %f2 %f0
	addi	%r9 %r9 $-1
	jr	%r8
min_caml_float_of_int_sum_m_exit:
	fadd	%f0 %f1 %f2
	jr	%r31
# cos
min_caml_cos:
	# 定義域を[0, 2pi)にする
	# r8: FLAG, r9: addr
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
	# r8: flag, r9: addr
	# f0: answer, f1: x^2, f3: const
	fmul	%f1 %f0 %f0
	addi	%r9 %r0 min_caml_kernel_cos_c3
	fld	0(%r9) %f3
	fmul	%f0 %f3 %f1
	addi	%r9 %r0 min_caml_kernel_cos_c2
	fld	0(%r9) %f3
	fadd	%f0 %f0 %f3
	fmul	%f0 %f0 %f2
	addi	%r9 %r0 min_caml_kernel_cos_c1
	fld	0(%r9) %f3
	fadd	%f0 %f0 %f2
	fmul	%f0 %f0 %f1
	addi	%r9 %r0 min_caml_float_1
	fld	0(%r9) %f3
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
	# 定義域を[0, 2pi)にする
	# r8: FLAG, r9: addr
	# f0: x, f1: pi, f2: 0.5, f3: temp(pi/2), f4: temp
	addi	%r9 min_caml_float_0
	fld	0(%r9) %f1
	fslt	%f0 %f1
	bclt	min_caml_sin_flag_negative
	addi	%r8 %r0 $0
	addi	%r9 min_caml_sin_after_flag
	jr	%r9
min_caml_sin_flag_negative:
	addi	%r8 %r0 $1
min_caml_sin_after_flag:
	fabs	%f0 %f0
	st	-1(%r29) %r8
	addi	%r29 %r29 $-2
	st	0(%r29) %r31
	jal	min_caml_reduction_2pi
	ld	0(%r29) %r31
	addi	%r29 %r29 $2
	ld	-1(%r29) %r8
	# x >= piならx := x - pi, FLAG reverse
	addi	%r9 %r0 min_caml_pi
	fld	0(%r9) %f1
	fslt	%f0 %f1
	bclt	min_caml_sin_2
	fneg	%f3 %f1
	fadd	%f0 %f0 %f3
	addi	%r8 %r8 $1
	addi	%r9 %r0 $1
	sll	%r8 %r8 %r9
min_caml_sin_2:
	# x >= pi/2ならx := pi - x
	addi	%r9 %r0 min_caml_half
	fld	0(%r9) %f2
	fmul	%f3 %f1 %f2
	fslt	%f0 %f3
	bclt	min_caml_sin_3
	fneg	%f4 %f0
	fadd	%f0 %f1 %f4
min_caml_sin_3:
	# x <= pi/4ならkernel_sin, そうでないならx := pi/2 - x, kernel_cosする
	fmul	%f4 %f3 %f2
	fslt	%f4 %f0
	bclf	min_caml_kernel_sin
	fneg	%f4 %f0
	fadd	%f0 %f3 %f4
	jr	min_caml_kernal_cos
min_caml_kernel_sin:
	# Tayler展開で計算する
	# r8: flag, r9: addr
	# f0: x or answer, f1: temp, f2: x^2, f3: const
	fmul	%f2 %f0 %f0
	addi	%r9 %r0 min_caml_sin_c3
	fld	0(%r9) %f3
	fmul	%f1 %f3 %f2
	addi	%r9 %r0 min_caml_sin_c2
	fld	0(%r9) %f3
	fadd	%f1 %f1 %f3
	fmul	%f1 %f1 %f2
	addi	%r9 %r0 min_caml_sin_c1
	fld	0(%r9) %f3
	fadd	%f1 %f1 %f3
	fmul	%f1 %f1 %f2
	fmul	%f0 %f1 %f0
	beq	%r8 %r0 min_caml_kernel_sin_positive
	fabs	%f0 %f0
	fneg	%f0 %f0
	jr	%r31
min_caml_kernel_sin_positive:
	fabs	%f0 %f0
	jr	%r31
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
min_caml_reduction_2pi_after_if:
	fmul	%f3 %f3 %f2
min_caml_reduction_2pi_exit:
	jr	%r31
# atan
min_caml_atan:
	# r8: FLAG, r9: addr
	# f0: x, f1: pi, f2: 0.5, f3: temp(pi/4, pi/2), f4: temp, f5: temp
	addi	%r9 min_caml_float_0
	fld	0(%r9) %f1
	fslt	%f0 %f1
	bclt	min_caml_atan_flag_negative
	addi	%r8 %r0 $0
	addi	%r9 min_caml_atan_after_flag
	jr	%r9
min_caml_atan_flag_negative:
	addi	%r8 %r0 $1
min_caml_atan_after_flag:
	fabs	%f0 %f0
	addi	%r9 %r0 min_caml_atan_c1
	fld	0(%9) %f4
	fslt	%f0 %f4
	bclf	min_caml_atan_2
	# |x| < 0.4375
	# kernel_atan(|x|)を返す
	addi	%r29 %r29 $-1
	st	0(%r29) %r31
	jal	min_caml_kernel_atan
	ld	0(%r29) %r31
	addi	%r29 %r29 $1
	beq	%r8 %r0 min_caml_atan_positive
	fabs	%f0 %f0
	fneg	%f0 %f0
	jr	%r31
min_caml_atan_positive:
	fabs	%f0 %f0
	jr	%r31
min_caml_atan_2:
	addi	%r9 %r0 min_caml_atan_c2
	fld	0(%r9) %f4
	fslt	%f0 %f4
	bclf	min_caml_atan_3
	# 0.4375 <= |x| < 2.4375
	# pi/4 + kernel_atan((|x|+1)/(|x|-1)) = kernel_atan(|x|)を返す
	addi	%r9 %r0 min_caml_float_1
	fld	0(%r9) %f3
	fadd	%f4 %f0 %f3
	fneg	%f3 %f3
	fadd	%f5 %f0 %f3
	finv	%f5 %f5
	fmul	%f0 %f4 %f5
	addi	%r29 %r29 $-1
	st	0(%r29) %r31
	jal	min_caml_kernel_atan
	ld	0(%r29) %r31
	addi	%r29 %r29 $1
	addi	%r9 %r0 min_caml_pi
	fld	0(%r9) %f1
	addi	%r9 %r0 min_caml_half
	fld	0(%r9) %f2
	fmul	%f3 %f1 %f2
	fmul	%f3 %f3 %f2
	fadd	%f0 %f0 %f3
	beq	%r8 %r0 min_caml_atan_positive
	fabs	%f0 %f0
	fneg	%f0 %f0
	jr	%r31
min_caml_atan_3:
	# |x| >= 2.4375
	# pi/2 - kernel_atan(1/|x|) = kernel_atan(|x|)を返す
	finv	%f0 %f0
	addi	%r29 %r29 $-1
	st	0(%r29) %r31
	jal	min_caml_kernel_atan
	ld	0(%r29) %r31
	addi	%r29 %r29 $1
	fneg	%f4 %f0
	addi	%r9 %r0 min_caml_pi
	fld	0(%r9) %f1
	addi	%r9 %r0 min_caml_half
	fld	0(%r9) %f2
	fmul	%f0 %f1 %f2
	fadd	%f0 %f0 %f4
	beq	%r8 %r0 min_caml_atan_positive
	fabs	%f0 %f0
	fneg	%f0 %f0
	jr	%r31
min_caml_kernel_atan:
	# Tayler展開で計算する
	# r8には触らないようにする, r9: addr
	# f0: x or answer, f1: temp, f2: x^2, f3: const
	fmul	%f2 %f0 %f0
	addi	%r9 %r0 min_caml_kernel_atan_c6
	fld	0(%r9) %f3
	fmul	%f1 %f3 %f2
	addi	%r9 %r0 min_caml_kernel_atan_c5
	fld	0(%r9) %f3
	fadd	%f1 %f1 %f3
	fmul	%f1 %f1 %f2
	addi	%r9 %r0 min_caml_kernel_atan_c4
	fld	0(%r9) %f3
	fadd	%f1 %f1 %f3
	fmul	%f1 %f1 %f2
	addi	%r9 %r0 min_caml_kernel_atan_c3
	fld	0(%r9) %f3
	fadd	%f1 %f1 %f3
	fmul	%f1 %f1 %f2
	addi	%r9 %r0 min_caml_kernel_atan_c2
	fld	0(%r9) %f3
	fadd	%f1 %f1 %f3
	fmul	%f1 %f1 %f2
	addi	%r9 %r0 min_caml_kernel_atan_c1
	fld	0(%r9) %f3
	fadd	%f1 %f1 %f3
	fmul	%f1 %f1 %f2
	fmul	%f0 %f1 %f0
	jr	%r31

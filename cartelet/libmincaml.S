.text
.globl min_caml_print_newline
min_caml_print_newline:
	# LF is 0x0a = 10
	addi %r1 %r0 $10
	send8 %r1
	jr %r31
.globl min_caml_print_int
min_caml_print_int:
	slt %r1 %r16 %r0
	beq %r1 %r0 min_caml_print_int_positive
	# '-' is 0x2d = 45
	addi %r1 %r0 $45
	send8 %r1
min_caml_print_int_positive:
	# 1桁ずつsend8する(* あとでやる *)
	send8 %r16
	jr %r31
.globl min_caml_print_byte
min_caml_print_byte:
	send8 %r16
	jr %r31
.globl min_caml_prerr_int
min_caml_prerr_int:
	addi %r1 %r0 min_caml_print_int
	jr %r1
.globl min_caml_prerr_byte
min_caml_prerr_byte:
	addi %r1 %r0 min_caml_print_byte
	jr %r1
.globl min_caml_prerr_float
min_caml_prerr_float:
	# 後でやる
	jr %r31
.globl min_caml_read_int
min_caml_read_int:
	# 未実装
	jr %r31
.globl min_caml_read_float
min_caml_read_float:
	# 未実装
	jr %r31
.globl min_caml_create_array
min_caml_create_array:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	pushl	%edx
	movl	%eax, %ecx
	movl	min_caml_hp, %eax
	movl	%ecx, %edx
	shll	$2, %edx
	addl	%edx, min_caml_hp
create_array_loop:
	cmpl	$0, %ecx
	jne	create_array_cont
create_array_exit:
	popl	%edx
	popl	%ecx
	movl	%ebp, %esp
	popl	%ebp
	ret
create_array_cont:
	decl	%ecx
	movl	%ebx, (%eax,%ecx,4)
	jmp	create_array_loop
.globl min_caml_create_float_array
min_caml_create_float_array:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	pushl	%edx
	movl	%eax, %ecx
	movl	min_caml_hp, %eax
	movl	%ecx, %edx
	shll	$3, %edx
	addl	%edx, min_caml_hp
create_float_array_loop:
	cmpl	$0, %ecx
	jne	create_float_array_cont
create_float_array_exit:
	popl	%edx
	popl	%ecx
	movl	%ebp, %esp
	popl	%ebp
	ret
create_float_array_cont:
	decl	%ecx
	movsd	%xmm0, (%eax,%ecx,8)
	jmp	create_float_array_loop
.globl min_caml_abs_float
min_caml_abs_float:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(fabs)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_sqrt
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
.globl min_caml_hp
min_caml_hp:	
	.long	0x0

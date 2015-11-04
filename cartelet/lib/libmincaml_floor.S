# ffloor
min_caml_floor:
	# float_of_int( int_of_float( x ) )をする
	# |x| < 8388608ならもっと速くできるがとりあえず実装
	# f0: x
	fst	-1(%r29) %f0
	addi	%r29 %r29 $-2
	st	0(%r29) %r31
	jal	min_caml_int_of_float
	jal	min_caml_float_of_int
	ld	0(%r29) %r31
	addi	%r29 %r29 $2
	fld	1(%r29) %f1
	# f0: float_of_int(int_of_float(x)), f1: x
	fseq	%r8 %r16
	bclt	min_caml_floor_exit
	addi	%r8 %r0 min_caml_float_minus_1
	fld	0(%r8) %f2
	fadd	%f1 %f1 %f2
	fslt	%f1 %f0
	bclf	min_caml_floor_exit
	fmov	%f0 %f1
min_caml_floor_exit:
	jr	%r31

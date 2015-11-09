# ffloor
min_caml_floor:
	# float_of_int( int_of_float( x ) )をする
	# |x| >= 8388608のとき元々整数
	# |x| < 8388608のときもっと速くできるがとりあえず実装
	# f0: x
	addi	%r8 %r0 min_caml_float_int_c2
	fld	0(%r8) %f1
	fslt	%f0 %f1
	bclf	min_caml_floor_exit
	addi	%r8 %r0 min_caml_float_int_c1
	fld	0(%r8) %f1
	fslt	%f1 %f0
	bclf	min_caml_floor_exit
min_caml_floor_small:
	fst	-1(%r29) %f0
	addi	%r29 %r29 $-2
	st	0(%r29) %r31
	jal	min_caml_int_of_float
	jal	min_caml_float_of_int
	ld	0(%r29) %r31
	addi	%r29 %r29 $2
	fld	-1(%r29) %f1
	# f0: float_of_int(int_of_float(x)), f1: x, f2: -1.0f, f3: 0.0f, f4: 1.0f
	# x<0ならfloor(x)+1を求めているので-1する
	addi	%r8 %r0 min_caml_float_minus_1
	fld	0(%r8) %f2
	addi	%r8 %r0 min_caml_float_0
	fld	0(%r8) %f3
	fslt	%f1 %f3
	bclf	min_caml_floor_positive
	fadd	%f0 %f0 %f2
min_caml_floor_negative:
	# x < %f0 < x+1となっているときは+1する
	fslt	%f1 %f0
	bclf	min_caml_floor_exit
	addi	%r8 %r0 min_caml_float_1
	fld	0(%r8) %f4
	fadd	%f1 %f1 %f4
	fslt	%f0 %f1
	bclf	min_caml_floor_exit
	fadd	%f0 %f0 %f4
min_caml_floor_positive:
	# x-1 < %f0 < xとなっているときは-1する
	fslt	%f0 %f1
	bclf	min_caml_floor_exit
	fadd	%f1 %f1 %f2
	fslt	%f1 %f0
	bclf	min_caml_floor_exit
	fadd	%f0 %f0 %f2
min_caml_floor_exit:
	jr	%r31

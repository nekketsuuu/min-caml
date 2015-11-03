# read_int
min_caml_read_int:
	addi	%r8 %r0 $8
	recv8	%r2
	sll	%r2 %r2 %r8
	recv8	%r2
	sll	%r2 %r2 %r8
	recv8	%r2
	sll	%r2 %r2 %r8
	recv8	%r2
	jr	%r31
# read_float
min_caml_read_float:
	addi	%r8 %r0 $8
	recv8	%r2
	sll	%r2 %r2 %r8
	recv8	%r2
	sll	%r2 %r2 %r8
	recv8	%r2
	sll	%r2 %r2 %r8
	recv8	%r2
	st	-1(%r29) %r2
	fld	-1(%r29) %f0
	jr	%r31

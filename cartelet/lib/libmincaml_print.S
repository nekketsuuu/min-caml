# print_newline
min_caml_print_newline:
	# LF is 0x0a = 10
	addi	%r8 %r0 $10
	send8	%r8
	jr	%r31
# print_char (print_byte)
min_caml_print_byte:
min_caml_print_char:
	send8	%r2
	jr	%r31
# print_int(32bit)
min_caml_print_int:
	addi	%r8 %r0 $8
	send8	%r2
	srl	%r2 %r2 %r8
	send8	%r2
	srl	%r2 %r2 %r8
	send8	%r2
	srl	%r2 %r2 %r8
	send8	%r2
	jr	%r31
# print_float(32bit)
min_caml_print_float:
	fst	-1(%r29) %f0
	ld	-1(%r29) %r8
	addi	%r9 %r0 $8
	send8	%r8
	srl	%r8 %r9 %r9
	send8	%r8
	srl	%r8 %r9 %r9
	send8	%r8
	srl	%r8 %r8 %r9
	send8	%r8
	jr	%r31	

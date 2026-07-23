let runtime =
  {|.text
.global main
alloc_closure:
	# input: a0 - codeptr, a1 - arity
	addi sp, sp, -32
	sd ra, 24(sp)
	sd s0, 16(sp)
	sd s1, 8(sp)
	mv s1, a0
	mv s0, a1

	slli a1, a1, 3 	# a1 *= 8
	addi a1, a1, 16	
	li a0, 1
	call calloc@plt
	sw s0, 0(a0)
	sd s1, 8(a0)

	ld ra, 24(sp)
	ld s0, 16(sp)
	ld s1, 8(sp)
	addi sp, sp, 32
	# output: a0 - closure
	ret
applyN:
	# input: a0 - closure ptr, a1 - data ptr, a2 - data size
	addi sp, sp, -48
	sd ra, 40(sp)
	sd s0, 32(sp)
	sd s1, 24(sp)
	sd s2, 16(sp)
	sd s3, 8(sp)

	mv s0, a0
	mv s3, a1
	mv s1, a2
	mv a1, a2
	li a0, 1
	call calloc@plt
	mv s2, a0
	mv a2, s1
	mv a1, s3
	call memcpy@plt
	lw a5, 4(s0) # load arg_received
	addiw a4, a5, 1
	sw a4, 4(s0)
	
	slli a5, a5, 3
	addi a5, a5, 16
	add	s0, s0, a5
	sd s2, 0(s0) # save data_copy_ptr in closure

	ld ra, 40(sp)
	
  sub	s0, s0, a5
	lw t1, 0(s0)
	beq a4, t1, applyN_result

	
	ld s0, 32(sp)
	ld s1, 24(sp)
	ld s2, 16(sp)
	ld s3, 8(sp)
	addi sp, sp, 48
	ret
applyN_result:
	# t0 - counter, t1 - amount args
	li t0, 0
	beq t0, t1, applyN_call

	ld a0, 16(s0)
	ld a0, 0(a0)
	addi t0, t0, 1
	beq t0, t1, applyN_call

	ld a1, 24(s0)
	ld a1, 0(a1) 
	addi t0, t0, 1
	beq t0, t1, applyN_call

	ld a2, 32(s0)
	ld a2, 0(a2) 
	addi t0, t0, 1
	beq t0, t1, applyN_call

	ld a3, 40(s0) 
	ld a3, 0(a3) 
	addi t0, t0, 1
	beq t0, t1, applyN_call

	ld a4, 48(s0)
	ld a4, 0(a4) 
	addi t0, t0, 1
	beq t0, t1, applyN_call

	ld a5, 56(s0) 
	ld a5, 0(a5) 
	addi t0, t0, 1
	beq t0, t1, applyN_call

	ld a6, 64(s0) 
	ld a6, 0(a6) 
	addi t0, t0, 1
	beq t0, t1, applyN_call

	ld a7, 72(s0) 
	ld a7, 0(a7) 
	addi t0, t0, 1
	beq t0, t1, applyN_call

	li a7, 94
	li a0, 53
	ecall
applyN_call:
	ld t2, 8(s0)

	addi sp, sp, 48
	jalr zero, 0(t2)
|}

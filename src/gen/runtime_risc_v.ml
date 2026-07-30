(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov *)

let runtime =
  {|.text
.global main
closure_alloc:
	# Description:
	# Allocates memory on heap for closure and fills: 1) codeptr; 2) arity, - fields
	# Arguments:
	# a0 - codeptr, a1 - arity
	addi sp, sp, -32
	sd ra, 0(sp)
	sd a0, 8(sp)
	sw a1, 16(sp)

	li a0, 1
	slli a1, a1, 3 	# a1 *= 8
	addi a1, a1, 16	
	call calloc@plt
	mv t0, a0 # t0 - allocated ptr 

	ld ra, 0(sp)
	ld a0, 8(sp)
	lw a1, 16(sp)

	sd a0, 8(t0) # save codeptr
	sw a1, 0(t0) # save arity

	addi sp, sp, 32
	mv a0, t0
	# Output: a0 - closure
	ret
closure_copy:
	# Input: a0 - closure ptr
	addi sp, sp, -32
	
	lw a1, 0(a0)
	slli a1, a1, 3 	# a1 *= 8
	addi a1, a1, 16	
	sw a1, 0(sp)
	sd a0, 8(sp)
	sd ra, 16(sp)

	li a0, 1
	call calloc@plt

	ld a1, 8(sp) # load closure ptr
	lw a2, 0(sp) # load data size
	call memcpy@plt

	ld ra, 16(sp)
	addi sp, sp, 32
	# Output: a0 - ptr to clonned closure data
	ret
closure_apply:
	# Description:
	# Applies 1 argument to closure. 
	# If all arguments are passed, then it will call closure's function.
	# Arguments:
	# a0 - closure, a1 - ptr to data
	addi sp, sp, -32
	sd ra, 0(sp)
	sd s0, 8(sp)
	sd s1, 16(sp)
	sd s2, 24(sp)

	lw t0, 4(a0) # load amount args received
	addiw t1, t0, 1
	sw t1, 4(a0)

	slli a2, t1, 3
	addi a2, a2, 16 # t1 - closure size in bytes

	mv s0, a0
	mv s1, a1
	mv s2, a2
	
	li a0, 1
	mv a1, a2
	call calloc@plt

	add t0, s0, s2 
	addi t0, t0, -8 # t0 - address, where must be n-th arg ptr 
	sd a0, 0(t0)

	mv a1, s1 # load closure ptr
	mv a2, s2 # load data size
	call memcpy@plt

	lw t0, 0(s0) # t1 - arity
	lw t1, 4(s0) # t2 - accepted

	mv a0, s0
	ld ra, 0(sp)
	ld s0, 8(sp)
	ld s1, 16(sp)
	ld s2, 24(sp)
	addi sp, sp, 32

	beq t0, t1, closure_args_sub
	# Output may be: 1) result of closure function; 2) nothing
	ret

closure_args_sub:
	# Description:
	# Substitutes closure's arguments into registers.
	# Arguments:
	# a0 - closure ptr
	addi sp, sp, -16
	sd s0, 0(sp)
	mv s0, a0

	lw t1, 0(s0) # t1 - amount args

	li t0, 0
	beq t0, t1, closure_call
	ld a0, 16(s0)
	ld a0, 0(a0)

	li t0, 1
	beq t0, t1, closure_call
	ld a1, 24(s0)
	ld a1, 0(a1)

	li t0, 2
	beq t0, t1, closure_call
	ld a2, 32(s0)
	ld a2, 0(a2) 

	li t0, 3
	beq t0, t1, closure_call
	ld a3, 40(s0) 
	ld a3, 0(a3) 

	li t0, 4
	beq t0, t1, closure_call
	ld a4, 48(s0)
	ld a4, 0(a4) 

	li t0, 5
	beq t0, t1, closure_call
	ld a5, 56(s0) 
	ld a5, 0(a5)

	li t0, 6
	beq t0, t1, closure_call
	ld a6, 64(s0)
	ld a6, 0(a6)

	li t0, 7
	beq t0, t1, closure_call
	ld a7, 72(s0)
	ld a7, 0(a7)

	li t0, 8
	beq t0, t1, closure_call

	li a7, 94
	li a0, 53
	ecall

closure_call:
	ld t0, 8(s0)
	ld s0, 0(sp)

	addi sp, sp, 16
	jalr zero, 0(t0)
|}

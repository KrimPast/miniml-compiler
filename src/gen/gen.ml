(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Asm
open Exprs
open Printf

exception GenError of string

type context = {
  mutable function_name : string;
  to_return_stack : string Stack.t;
  mutable has_callings : bool;
  mutable stack_size : int;
  mutable amount_of_if : int;
}

let ct =
  {
    function_name = "main";
    to_return_stack = Stack.create ();
    has_callings = false;
    stack_size = 16;
    amount_of_if = 0;
  }

let arg_regs = ref [ "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" ]
let temp_regs = ref [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7" ]
let reg_table = Hashtbl.create 16

(* Useful ideas:
General: 
- Scope (it may be function, begin ... end and etc.)

Optimization:
- Store register value (to avoid repeated assignment the same value) 
*)

let print_table () =
  Hashtbl.iter (fun key value -> Printf.printf "%s -> %s\n" key value) reg_table

let is_persistent_reg reg =
  Hashtbl.fold (fun _ v acc -> acc || v = reg) reg_table false

let free_register reg =
  if not (is_persistent_reg reg) then
    begin if List.exists (fun x -> x = reg) !temp_regs then begin
      print_table ();
      raise
        (GenError
           (sprintf "free_register: Free non-allocatable temp register '%s'" reg))
    end
    else temp_regs := reg :: !temp_regs
      (* print_endline @@ "Deallocated reg: " ^ reg *)
    end
  else
    begin match Hashtbl.find_opt reg_table reg with
    | Some _ -> Hashtbl.remove reg_table reg
    | None ->
        raise
          (GenError
             (sprintf
                "free_register: Free non-allocatable argument register '%s'" reg))
    end

let get_free_register regs =
  if List.length !regs = 0 then
    raise @@ GenError "get_free_register: Not enough registers!"
  else begin
    let reg = List.hd !regs in
    regs := List.tl !regs;
    (* print_endline @@ "Allocated reg: " ^ reg; *)
    reg
  end

let is_has_register_to_return () = not (Stack.is_empty ct.to_return_stack)

let alloc_and_push_reg () =
  let rs = get_free_register temp_regs in
  Stack.push rs ct.to_return_stack;
  rs

let pop_and_check_reg rs =
  if Stack.is_empty ct.to_return_stack then
    raise @@ GenError "pop_and_check_reg: Return-stack is empty";

  let maybe_rs = Stack.pop ct.to_return_stack in
  if maybe_rs <> rs then
    raise
      (GenError
         (sprintf "pop_and_check_reg: Expected register %s instead of %s" rs
            maybe_rs))

(* let rec is_has_certain_expr predicate x =
  if predicate x then true
  else 
    match x with
    | EFunc(_, _, body) -> is_has_certain_expr predicate body
    | EIf(cond, thn, els) -> is_has_certain_expr predicate cond ||
                            is_has_certain_expr predicate thn || 
                            is_has_certain_expr predicate els 
    | EBinop(_, left, right) -> is_has_certain_expr predicate left ||
                                is_has_certain_expr predicate right
    | ESeqLocal(curr, next) ->  is_has_certain_expr predicate curr ||
                                is_has_certain_expr predicate next
    | ECond(left, _, right) -> is_has_certain_expr predicate left ||
                              is_has_certain_expr predicate right
    | ELet(_, expr) -> is_has_certain_expr predicate expr
    | _ -> raise @@ GenError "is_has_certain_expr: Not implemented" *)
let rec generate_code = function
  | EFunc (name, args, body) ->
      ct.function_name <- name;
      ct.has_callings <- false;
      ct.amount_of_if <- 0;

      List.iter
        (fun arg ->
          let new_reg = get_free_register arg_regs in
          Hashtbl.add reg_table arg new_reg)
        args;

      Stack.push "a0" ct.to_return_stack;
      let body = generate_code body in
      pop_and_check_reg "a0";

      (* If outer callings is exist in this function, then save our arguments *)
      if ct.has_callings = true then begin
        ct.stack_size <- ((Hashtbl.length reg_table / 2) + 1) * 16;
        let alloc_frame = str_of_instr_w (ADDI ("sp", "sp", -ct.stack_size)) in
        let dealloc_frame = str_of_instr_w (ADDI ("sp", "sp", ct.stack_size)) in

        str_of_instr_w (LABEL name)
        ^ alloc_frame ^ body ^ dealloc_frame ^ str_of_instr_w RET
      end
      else str_of_instr_w (LABEL name) ^ body ^ str_of_instr_w RET
  | EIf (cond, thn, els) -> begin
      ct.amount_of_if <- ct.amount_of_if + 1;
      let then_name =
        ct.function_name ^ "_then_" ^ string_of_int ct.amount_of_if
      in
      let fin_name =
        ct.function_name ^ "_fin_" ^ string_of_int ct.amount_of_if
      in
      let then_label = str_of_instr_w (LABEL then_name) in
      let fin_label = str_of_instr_w (LABEL fin_name) in
      let jump_final = str_of_instr_w (J fin_name) in
      let condition = generate_code cond in

      let then_code = generate_code thn in
      let else_code = generate_code els in

      condition ^ else_code ^ jump_final ^ then_label ^ then_code ^ fin_label
    end
  | EBinop (op, left, right) ->
      let rs1 = alloc_and_push_reg () in
      let left_code = generate_code left in
      pop_and_check_reg rs1;

      let rs2 = alloc_and_push_reg () in
      let right_code = generate_code right in
      pop_and_check_reg rs2;

      free_register rs1;
      free_register rs2;

      let rd = Stack.top ct.to_return_stack in
      let inst =
        begin match op with
        | Add -> str_of_instr_w (ADD (rd, rs1, rs2))
        | Sub -> str_of_instr_w (SUB (rd, rs1, rs2))
        | Multiply -> str_of_instr_w (MUL (rd, rs1, rs2))
        | Divide -> str_of_instr_w (DIV (rd, rs1, rs2))
        end
      in
      left_code ^ right_code ^ inst
  | ESeq (curr, next) ->
      let curr_code = generate_code curr in
      let next_code = generate_code next in
      curr_code ^ next_code
  | ESeqLocal (curr, next) ->
      let curr_code = generate_code curr in
      let next_code = generate_code next in
      curr_code ^ next_code
  | ENum num ->
      let rd = Stack.top ct.to_return_stack in
      str_of_instr_w (LI (rd, num))
  | EVar name ->
      let rd = Stack.top ct.to_return_stack in
      let rs =
        begin match Hashtbl.find_opt reg_table name with
        | Some v -> v
        | None -> raise @@ GenError (sprintf "Unitialized variable '%s'" name)
        end
      in
      str_of_instr_w (MV (rd, rs))
  | ECond (left, op, right) ->
      let label_name =
        ct.function_name ^ "_then_" ^ string_of_int ct.amount_of_if
      in
      let left_res = alloc_and_push_reg () in
      let left_code = generate_code left in
      pop_and_check_reg left_res;

      let right_res = alloc_and_push_reg () in
      let right_code = generate_code right in
      pop_and_check_reg right_res;

      free_register left_res;
      free_register right_res;

      left_code ^ right_code
      ^ begin match op with
      | TGe -> str_of_instr_w (BGE (left_res, right_res, label_name))
      | TGt -> str_of_instr_w (BGT (left_res, right_res, label_name))
      | TLt -> str_of_instr_w (BLT (left_res, right_res, label_name))
      | TLe -> str_of_instr_w (BLE (left_res, right_res, label_name))
      | TEq -> str_of_instr_w (BEQ (left_res, right_res, label_name))
      | TNe -> str_of_instr_w (BNE (left_res, right_res, label_name))
      | _ ->
          raise @@ GenError "Expected one of '<=', '<', '>', '>=' in condition."
      end
  | ELet (name, expr) ->
      let rd =
        match Hashtbl.find_opt reg_table name with
        | Some v ->
            raise @@ GenError (sprintf "ELet: Double allocating reg %s" v)
        | None ->
            let new_reg = get_free_register arg_regs in
            Hashtbl.add reg_table name new_reg;
            (* print_endline (sprintf "ELet: Allocated %s" new_reg); *)
            (* print_table(); *)
            new_reg
      in
      Stack.push rd ct.to_return_stack;
      let code = generate_code expr in
      pop_and_check_reg rd;
      code
  | EClosure (name, args) ->
      let alloc_stack = str_of_instr_w (ADDI ("sp", "sp", -48)) in
      let alloc_closure =
        str_of_instr_w (LA ("a0", name))
        ^ str_of_instr_w (LI ("a1", List.length args))
        ^ str_of_instr_w (CALL "alloc_closure")
        ^ str_of_instr_w (MV ("s0", "a0"))
      in

      let pos = ref 8 in
      let saved_regs = ref (str_of_instr_w (SD ("ra", 0, "sp"))) in
      let loaded_regs = ref (str_of_instr_w (LD ("ra", 0, "sp"))) in
      reg_table |> Hashtbl.to_seq_values |> List.of_seq
      (* Convert Seq.t to list *) |> List.sort String.compare
      |> List.iter (fun value ->
          saved_regs := !saved_regs ^ str_of_instr_w (SD (value, !pos, "sp"));
          (* Если результат функции нужно положить в регистр x, то его сохранять и восстанавливать не нужно *)
          if value <> "a0" then begin
            loaded_regs :=
              !loaded_regs ^ str_of_instr_w (LD (value, !pos, "sp"))
          end;
          pos := !pos + 8);

      let load_a0 = str_of_instr_w (LD ("a0", 8, "sp")) in

      (* let save_a0 = str_of_instr_w (SD ("a0", !pos, "sp")) in *)
      let args_len = List.length args in
      let arg_i = ref 0 in
      let s0_location = !pos in
      let data_location = !pos + 8 in

      let args_str =
        List.map
          (fun arg ->
            let applyN_args =
              str_of_instr_w (ADDI ("a1", "sp", data_location))
              ^ str_of_instr_w (LI ("a2", 8))
            in

            let rs = alloc_and_push_reg () in
            let arg_code = generate_code arg in
            pop_and_check_reg rs;
            free_register rs;
            let rs_save = str_of_instr_w (SD (rs, 0, "a1")) in

            let load_closure = str_of_instr_w (MV ("a0", "s0")) in
            let load_a0_curr = if !arg_i <> args_len - 1 then load_a0 else "" in
            arg_i := !arg_i + 1;

            arg_code ^ applyN_args ^ rs_save ^ load_closure
            ^ str_of_instr_w (CALL "applyN")
            ^ load_a0_curr ^ !loaded_regs)
          args
      in
      let args_code = String.concat "" args_str in
      let dealloc_stack = str_of_instr_w (ADDI ("sp", "sp", 48)) in

      alloc_stack
      ^ str_of_instr_w (SD ("s0", s0_location, "sp"))
      ^ !saved_regs ^ alloc_closure ^ load_a0 ^ !loaded_regs ^ args_code
      ^ str_of_instr_w (LD ("s0", s0_location, "sp"))
      ^ dealloc_stack
  | ECall (name, args) ->
      ct.has_callings <- true;

      let rd = Stack.top ct.to_return_stack in
      let code = generate_code (EClosure (name, args)) in
      (* Проблема в том, что если положить результат от вызова функции в a0, 
    то нынешний аргумент a0 перезатрётся. Поэтому сразу после получения перекладываем результат во временный регистр,
    а аргумент восстанавливаем со стека *)
      let pos = ref 8 in
      let saved_regs = ref (str_of_instr_w (SD ("ra", 0, "sp"))) in
      let loaded_regs = ref (str_of_instr_w (LD ("ra", 0, "sp"))) in
      reg_table |> Hashtbl.to_seq
      |> Seq.iter (fun (_, value) ->
          (* Если результат функции нужно положить в регистр x, то его сохранять и восстанавливать не нужно *)
          if value <> rd then begin
            saved_regs := !saved_regs ^ str_of_instr_w (SD (value, !pos, "sp"));
            loaded_regs :=
              !loaded_regs ^ str_of_instr_w (LD (value, !pos, "sp"))
          end;
          pos := !pos + 8);
      let move_res = str_of_instr_w (MV (rd, "a0")) in

      !saved_regs ^ code ^ move_res ^ !loaded_regs
  | ENothing -> ""

let generate_program expr =
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
  in
  let code = generate_code expr in
  runtime ^ code

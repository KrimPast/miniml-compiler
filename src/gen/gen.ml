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
  mutable has_closures : bool;
  mutable stack_size : int;
  mutable amount_of_if : int;
}

let symbol_table : (string, sexpr) Hashtbl.t = Hashtbl.create 128

let ct =
  {
    function_name = "main";
    to_return_stack = Stack.create ();
    has_callings = false;
    has_closures = false;
    stack_size = 16;
    amount_of_if = 0;
  }

let temp_regs = ref [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7" ]

let hash_table_reassign dest src =
  Hashtbl.clear dest;
  Hashtbl.iter (fun k v -> Hashtbl.replace dest k v) src

let get_registers_to_save () =
  Hashtbl.fold
    (fun _ v acc ->
      match v with
      | SVar vr ->
          begin match vr.placement with
          | SPlaceIsReg reg -> reg :: acc
          | _ -> acc
          end
      | _ -> acc)
    symbol_table []
  |> List.sort String.compare

let free_temp_register reg =
  if List.exists (fun x -> x = reg) !temp_regs then
    raise
      (GenError
         (sprintf "free_temp_register: Free non-allocatable temp register '%s'"
            reg))
  else temp_regs := reg :: !temp_regs

let get_free_temp_register () =
  if List.length !temp_regs = 0 then
    raise @@ GenError "get_free_temp_register: Not enough registers!"
  else begin
    let reg = List.hd !temp_regs in
    temp_regs := List.tl !temp_regs;
    reg
  end

let get_free_arg_register () =
  let busy_registers = get_registers_to_save () in

  let maybe_reg =
    List.find_opt
      (fun el -> not (List.mem el busy_registers))
      [ "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" ]
  in

  match maybe_reg with
  | Some reg -> reg
  | _ -> raise @@ GenError "Not enough argument registers!"

let is_has_register_to_return () = not (Stack.is_empty ct.to_return_stack)

let alloc_and_push_reg () =
  let rs = get_free_temp_register () in
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

let rec generate_code = function
  | EFunc (name, args, body) ->
      ct.function_name <- name;
      ct.has_callings <- false;
      ct.amount_of_if <- 0;

      let saved_symbol_table = Hashtbl.copy symbol_table in

      List.iter
        (fun arg ->
          let new_reg = get_free_arg_register () in
          Hashtbl.add symbol_table arg
            (SVar { placement = SPlaceIsReg new_reg }))
        args;

      Stack.push "a0" ct.to_return_stack;
      (* Add `if` to next stroke if this function is recursive *)
      Hashtbl.replace symbol_table name
        (SFunc { amount_args = List.length args });

      let body = generate_code body in

      ct.stack_size <-
        (((List.length @@ get_registers_to_save ()) / 2) + 1) * 16;
      hash_table_reassign symbol_table saved_symbol_table;
      pop_and_check_reg "a0";

      (* If outer callings is exist in this function, then save our arguments *)
      if ct.has_callings = true then begin
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

      free_temp_register rs1;
      free_temp_register rs2;

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
        begin match Hashtbl.find_opt symbol_table name with
        | Some found ->
            begin match found with
            | SVar { placement : splacement } ->
                begin match placement with
                | SPlaceIsReg reg -> reg
                | _ -> raise @@ GenError "SPlaceIsStack: Not implemented"
                end
            | _ -> raise @@ GenError "Expected variable, but got function"
            end
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

      free_temp_register left_res;
      free_temp_register right_res;

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
      begin match Hashtbl.find_opt symbol_table name with
      | Some _ ->
          (* По канону, присвоение тому же имени в miniML возможно, но пока ограничимся этим *)
          raise @@ GenError (sprintf "ELet: Attempt to reuse `%s` name." name)
      | None ->
          let rd = get_free_arg_register () in
          Stack.push rd ct.to_return_stack;
          let code = generate_code expr in

          Hashtbl.add symbol_table name (SVar { placement = SPlaceIsReg rd });
          pop_and_check_reg rd;
          code
      end
  | EClosure (name, args) ->
      ct.has_closures <- true;
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

      let to_save = get_registers_to_save () in

      List.iter
        (fun value ->
          saved_regs := !saved_regs ^ str_of_instr_w (SD (value, !pos, "sp"));
          (* Если результат функции нужно положить в регистр x, то его сохранять и восстанавливать не нужно *)
          if value <> "a0" then begin
            loaded_regs :=
              !loaded_regs ^ str_of_instr_w (LD (value, !pos, "sp"))
          end;
          pos := !pos + 8)
        to_save;

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
            free_temp_register rs;
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
      let is_closure =
        begin match Hashtbl.find_opt symbol_table name with
        | Some v ->
            begin match v with
            | SFunc { amount_args } ->
                let put_args = List.length args in
                if put_args < amount_args then true
                else if put_args = amount_args then false
                else
                  raise
                  @@ GenError
                       (sprintf
                          "Attempt to put %d arguments to function `%s`, but \
                           it has %d."
                          put_args name amount_args)
            | _ ->
                raise
                @@ GenError
                     (sprintf "Attempt to call variable `%s` as function" name)
            end
        | _ -> raise @@ GenError (sprintf "Function `%s` is not found" name)
        end
      in

      let code =
        if is_closure then generate_code (EClosure (name, args))
        else begin
          let arg_i = ref 0 in
          let args_str =
            List.map
              (fun arg ->
                let rs = alloc_and_push_reg () in
                let arg_str = generate_code arg in
                pop_and_check_reg rs;
                free_temp_register rs;
                let arg_res_move =
                  str_of_instr_w (MV ("a" ^ string_of_int !arg_i, rs))
                in
                arg_i := !arg_i + 1;
                arg_str ^ arg_res_move)
              args
          in
          let args_code = String.concat "" args_str in
          args_code ^ str_of_instr_w (CALL name)
        end
      in
      (* Проблема в том, что если положить результат от вызова функции в a0, 
    то нынешний аргумент a0 перезатрётся. Поэтому сразу после получения перекладываем результат во временный регистр,
    а аргумент восстанавливаем со стека *)
      let pos = ref 8 in
      let saved_regs = ref (str_of_instr_w (SD ("ra", 0, "sp"))) in
      let loaded_regs = ref (str_of_instr_w (LD ("ra", 0, "sp"))) in

      let to_save = get_registers_to_save () in
      List.iter
        (fun value ->
          (* Если результат функции нужно положить в регистр x, то его сохранять и восстанавливать не нужно *)
          if value <> rd then begin
            saved_regs := !saved_regs ^ str_of_instr_w (SD (value, !pos, "sp"));
            loaded_regs :=
              !loaded_regs ^ str_of_instr_w (LD (value, !pos, "sp"))
          end;
          pos := !pos + 8)
        to_save;
      let move_res = str_of_instr_w (MV (rd, "a0")) in

      !saved_regs ^ code ^ move_res ^ !loaded_regs
  | ENothing -> ""

let generate_program expr =
  let runtime = if ct.has_closures then Runtime_risc_v.runtime else "" in
  let code = generate_code expr in
  runtime ^ code

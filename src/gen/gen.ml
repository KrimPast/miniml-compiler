(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov *)

open Asm
open Exprs
open Printf

exception GenError of string

type context = {
  mutable function_name : string;
  mutable current_var : string;
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
    current_var = "let_error";
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
  "ra"
  :: (Hashtbl.fold
        (fun _ v acc ->
          match v with
          | SVar vr -> (
              match vr.placement with SPlaceIsReg reg -> reg :: acc | _ -> acc)
          | SClosure vr -> (
              match vr.placement with SPlaceIsReg reg -> reg :: acc | _ -> acc)
          | _ -> acc)
        symbol_table []
     |> List.sort String.compare)

let stack_do_action (ac : stack_action) (predicate : string -> bool) =
  let regs_list = get_registers_to_save () in
  let reduced_regs_list = List.filter predicate regs_list in
  let pos = ref 0 in
  List.map
    (fun reg ->
      let curr =
        match ac with
        | StoreRegs -> str_of_instr_w (SD (reg, !pos, "sp"))
        | LoadRegs -> str_of_instr_w (LD (reg, !pos, "sp"))
      in
      pos := !pos + 8;
      curr)
    reduced_regs_list
  |> String.concat ""

let stack_do_all (_ : string) = true

let stack_do_all_but_not_certain_reg (reg : string) (certain_reg : string) =
  reg <> certain_reg

let stack_do_all_but_not_a0 (reg : string) =
  stack_do_all_but_not_certain_reg reg "a0"

let get_sexpr_or_fall name error_msg =
  match Hashtbl.find_opt symbol_table name with
  | Some x -> x
  | None -> raise @@ GenError error_msg

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
  else
    let reg = List.hd !temp_regs in
    temp_regs := List.tl !temp_regs;
    reg

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
  | EFunc { name; args; body; is_rec } ->
      ct.function_name <- name;
      ct.current_var <- name;
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
      let sfunc = SFunc { amount_args = List.length args } in
      if is_rec then Hashtbl.replace symbol_table name sfunc;
      let body = generate_code body in
      ct.stack_size <-
        (((List.length @@ get_registers_to_save ()) / 2) + 1) * 16;
      hash_table_reassign symbol_table saved_symbol_table;
      Hashtbl.replace symbol_table name sfunc;
      pop_and_check_reg "a0";
      (* If outer callings is exist in this function, then save our arguments *)
      if ct.has_callings = true then
        let alloc_frame = str_of_instr_w (ADDI ("sp", "sp", -ct.stack_size)) in
        let dealloc_frame = str_of_instr_w (ADDI ("sp", "sp", ct.stack_size)) in
        str_of_instr_w (LABEL name)
        ^ alloc_frame ^ body ^ dealloc_frame ^ str_of_instr_w RET
      else str_of_instr_w (LABEL name) ^ body ^ str_of_instr_w RET
  | EIf (cond, thn, els) ->
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
  | EBinop (op, left, right) ->
      if not (Stack.is_empty ct.to_return_stack) then (
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
          match op with
          | Add -> str_of_instr_w (ADD (rd, rs1, rs2))
          | Sub -> str_of_instr_w (SUB (rd, rs1, rs2))
          | Multiply -> str_of_instr_w (MUL (rd, rs1, rs2))
          | Divide -> str_of_instr_w (DIV (rd, rs1, rs2))
        in
        left_code ^ right_code ^ inst)
      else ""
  | ESeq (curr, next) ->
      let curr_code = generate_code curr in
      let next_code = generate_code next in
      curr_code ^ next_code
  | ESeqLocal (curr, next) ->
      let curr_code = generate_code curr in
      let next_code = generate_code next in
      curr_code ^ next_code
  | ENum num ->
      if not (Stack.is_empty ct.to_return_stack) then
        let rd = Stack.top ct.to_return_stack in
        str_of_instr_w (LI (rd, num))
      else ""
  | EVar name ->
      if not (Stack.is_empty ct.to_return_stack) then
        let rd = Stack.top ct.to_return_stack in
        let rs =
          match Hashtbl.find_opt symbol_table name with
          | Some found -> (
              match found with
              | SVar { placement : splacement } -> (
                  match placement with
                  | SPlaceIsReg reg -> reg
                  | _ -> raise @@ GenError "SPlaceIsStack: Not implemented")
              | _ -> raise @@ GenError "Expected variable, but got function")
          | None -> raise @@ GenError (sprintf "Unitialized variable '%s'" name)
        in
        str_of_instr_w (MV (rd, rs))
      else ""
  | ECond (left, op, right) -> (
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
      ^
      match op with
      | TGe -> str_of_instr_w (BGE (left_res, right_res, label_name))
      | TGt -> str_of_instr_w (BGT (left_res, right_res, label_name))
      | TLt -> str_of_instr_w (BLT (left_res, right_res, label_name))
      | TLe -> str_of_instr_w (BLE (left_res, right_res, label_name))
      | TEq -> str_of_instr_w (BEQ (left_res, right_res, label_name))
      | TNe -> str_of_instr_w (BNE (left_res, right_res, label_name))
      | _ ->
          raise @@ GenError "Expected one of '<=', '<', '>', '>=' in condition."
      )
  | ELet (name, expr) -> (
      ct.current_var <- name;
      match Hashtbl.find_opt symbol_table name with
      | Some _ ->
          (* По канону, присвоение тому же имени в miniML возможно, но пока ограничимся этим *)
          raise @@ GenError (sprintf "ELet: Attempt to reuse `%s` name." name)
      | None ->
          let rd = get_free_arg_register () in
          Stack.push rd ct.to_return_stack;
          let code = generate_code expr in
          if Hashtbl.find_opt symbol_table name = None then
            (* Костыль для реализации замыканий *)
            Hashtbl.add symbol_table name (SVar { placement = SPlaceIsReg rd });
          pop_and_check_reg rd;
          code)
  | EClosureAlloc func ->
      ct.has_closures <- true;

      let sexpr =
        get_sexpr_or_fall func
          (sprintf "Excepted that `%s` is initialized." func)
      in
      let args_length =
        match sexpr with
        | SFunc { amount_args } -> amount_args
        | _ ->
            raise @@ GenError (sprintf "Excepted that `%s` is function." func)
      in

      let load_func_address = str_of_instr_w (LA ("a0", func)) in
      let load_args_length = str_of_instr_w (LI ("a1", args_length)) in
      let call_closure_alloc = str_of_instr_w (CALL "closure_alloc") in

      let rd_move =
        if not (Stack.is_empty ct.to_return_stack) then
          str_of_instr_w (MV (Stack.top ct.to_return_stack, "a0"))
        else ""
      in
      load_func_address ^ load_args_length ^ call_closure_alloc ^ rd_move
  | EClosureCopy closure ->
      let sexpr =
        get_sexpr_or_fall closure
          (sprintf "Excepted that `%s` is initialized." closure)
      in
      let rd = Stack.top ct.to_return_stack in
      let place =
        begin match sexpr with
        | SClosure { placement; remaining_args } ->
            Hashtbl.replace symbol_table ct.current_var
              (SClosure { placement = SPlaceIsReg rd; remaining_args });
            placement
        | _ ->
            raise @@ GenError (sprintf "Expected that `%s` is closure." closure)
        end
      in
      let closure_reg =
        begin match place with
        | SPlaceIsReg x -> x
        | _ -> raise @@ GenError "Store on stack is not implemented"
        end
      in
      let move_closure_reg = str_of_instr_w (MV ("a0", closure_reg)) in
      let call_closure_copy = str_of_instr_w (CALL "closure_copy") in
      let move_copy = str_of_instr_w (MV (rd, "a0")) in
      move_closure_reg ^ call_closure_copy ^ move_copy
  | EClosureApply (closure, arg) ->
      (* считаем expr-аргумент, кладём его на стек
          грузим в a0 closure
          грузим в a1 адрес результата expr-а
      *)
      let alloc_stack = str_of_instr_w (ADDI ("sp", "sp", -16)) in
      let dealloc_stack = str_of_instr_w (ADDI ("sp", "sp", 16)) in

      let sexp =
        get_sexpr_or_fall closure
          (sprintf "Attempt to use closure `%s` before initialization" closure)
      in

      let place, remain_args =
        begin match sexp with
        | SClosure { placement; remaining_args } ->
            Hashtbl.replace symbol_table closure
              (SClosure { placement; remaining_args = remaining_args - 1 });
            (placement, remaining_args)
        | _ ->
            raise @@ GenError (sprintf "Expected that `%s` is closure." closure)
        end
      in
      let closure_reg =
        begin match place with
        | SPlaceIsReg x -> x
        | _ -> raise @@ GenError "Store on stack is not implemented"
        end
      in

      let load_closure = str_of_instr_w (MV ("a0", closure_reg)) in
      let load_data_ptr = str_of_instr_w (MV ("a1", "sp")) in
      let call_closure_apply = str_of_instr_w (CALL "closure_apply") in
      let rs = alloc_and_push_reg () in
      let arg_code = generate_code arg in
      pop_and_check_reg rs;
      free_temp_register rs;

      let move_res =
        if (not (Stack.is_empty ct.to_return_stack)) && remain_args = 0 then begin
          let rd = Stack.top ct.to_return_stack in
          Hashtbl.replace symbol_table ct.current_var
            (SVar { placement = SPlaceIsReg rd });
          str_of_instr_w (MV (rd, "a0"))
        end
        else ""
      in

      let arg_result_save = str_of_instr_w (SD (rs, 0, "a1")) in

      alloc_stack ^ arg_code ^ load_closure ^ load_data_ptr ^ arg_result_save
      ^ call_closure_apply ^ move_res ^ dealloc_stack
  | ECall (name, args) ->
      ct.has_callings <- true;
      if not (Stack.is_empty ct.to_return_stack) then begin
        let rd = Stack.top ct.to_return_stack in
        let sexp =
          get_sexpr_or_fall name (sprintf "Function `%s` is not found" name)
        in
        let closure_type =
          match sexp with
          | SClosure _ -> OldClosure
          | SFunc { amount_args } ->
              let put_args = List.length args in
              if put_args < amount_args then (
                Hashtbl.replace symbol_table ct.current_var
                  (SClosure
                     {
                       placement = SPlaceIsReg rd;
                       remaining_args = amount_args - put_args;
                     });
                NewClosure)
              else if put_args = amount_args then FullCall
              else
                raise
                @@ GenError
                     (sprintf
                        "Attempt to put %d arguments to function `%s`, but it \
                         has %d."
                        put_args name amount_args)
          | _ ->
              raise
              @@ GenError
                   (sprintf "Attempt to call variable `%s` as function" name)
        in

        let saved_regs =
          stack_do_action StoreRegs (stack_do_all_but_not_certain_reg rd)
        in
        let loaded_regs =
          stack_do_action LoadRegs (stack_do_all_but_not_certain_reg rd)
        in
        let move_res = str_of_instr_w (MV (rd, "a0")) in

        match closure_type with
        | NewClosure ->
            let closure_alloc = generate_code (EClosureAlloc name) in
            let new_saved_regs = stack_do_action StoreRegs stack_do_all in
            let new_loaded_regs = stack_do_action LoadRegs stack_do_all in
            let closure_args =
              List.map
                (fun arg ->
                  generate_code (EClosureApply (ct.current_var, arg))
                  ^ new_loaded_regs)
                args
            in
            saved_regs ^ closure_alloc ^ loaded_regs ^ new_saved_regs
            ^ String.concat "" closure_args
        | OldClosure ->
            let copy_closure = generate_code (EClosureCopy name) in
            let closure_args =
              List.map
                (fun arg ->
                  generate_code (EClosureApply (ct.current_var, arg))
                  ^ loaded_regs)
                args
            in
            saved_regs ^ copy_closure ^ loaded_regs
            ^ String.concat "" closure_args
        | FullCall ->
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
            saved_regs ^ args_code ^ str_of_instr_w (CALL name) ^ move_res
            ^ loaded_regs
      end
      else ""
  | ENothing -> ""

let generate_program expr =
  let code = generate_code expr in
  let runtime = if ct.has_closures then Runtime_risc_v.runtime else "" in
  if not (Stack.is_empty ct.to_return_stack) then (
    print_endline "WARNING: Return stack is not empty!";
    print_endline "WARNING: Stack elements:";
    Stack.iter (fun el -> print_endline el) ct.to_return_stack);
  runtime ^ code

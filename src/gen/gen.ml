(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Asm
open Exprs
open Printf

type context = {
  mutable function_name : string;
  mutable binop_returned : string;
  to_return_stack : string Stack.t;
  mutable has_callings : bool;
}
let ct = {
  function_name = "_main"; to_return_stack = Stack.create(); 
  binop_returned = "err"; has_callings = false
}
let arg_regs = ref [ "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" ]
let temp_regs = ref [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7" ]
let reg_table = Hashtbl.create 16
let print_table () = 
  Hashtbl.iter (fun key value -> Printf.printf "%s -> %s\n" key value) reg_table;;
let is_persistent_reg reg = 
  Hashtbl.fold (fun _ v acc -> acc || (v = reg)) reg_table false

let free_register regs reg =
  if not (is_persistent_reg reg) then begin
    if List.exists (fun x -> x = reg) !regs
    then begin 
      print_table ();
      failwith (sprintf "free_register: Free non-allocatable register '%s'" reg); 
    end
    else regs := reg :: !regs ;
    (* print_endline @@ "Deallocated reg: " ^ reg *)
  end;;
let get_free_register regs = 
  if List.length !regs = 0 then failwith "Not enough registers!"
  else begin
    let reg = List.hd !regs in
    regs := List.tl !regs;
    (* print_endline @@ "Allocated reg: " ^ reg; *)
    reg;
  end;;
let is_has_register_to_return () =
  not (Stack.is_empty ct.to_return_stack) 
let rec generate_code = function
| EFunc(name, body) ->
    ct.function_name <- name;
    Stack.push "a0" ct.to_return_stack;
    let body = generate_code body in
    let stack_out = Stack.pop ct.to_return_stack in
    if stack_out <> "a0" then failwith "Expected register a0 as result of function.";

    (* let res = str_of_instr_w (LABEL(name)) ^ generate_code body ^ str_of_instr_w RET in *)

    let save_ra = str_of_instr_w(SD("ra", 0, "sp")) in
    (* If outer callings is exist in this function, then save our arguments *)
    if ct.has_callings = true then
      let alloc_frame = str_of_instr_w (ADDI ("sp", "sp", -16)) in
      let dealloc_frame = str_of_instr_w (ADDI ("sp", "sp", 16)) in
      let saved_regs = ref "" in
      let pos = ref 8 in
      reg_table
      |> Hashtbl.to_seq
      |> Seq.iter (fun (_, value) -> 
        (* printf "Key: %s, value: %s\n" key value *)
        saved_regs := !saved_regs ^   str_of_instr_w(SD(value, !pos, "sp"));
        pos := !pos + 8;
      );
      str_of_instr_w (LABEL(name)) ^ alloc_frame ^ save_ra ^ !saved_regs ^ body ^ dealloc_frame ^ str_of_instr_w RET
    else
      str_of_instr_w (LABEL(name)) ^ body ^ str_of_instr_w RET
| EIf (cond, thn, els) ->
    begin  
      let then_name = ct.function_name ^ "_then" in
      let fin_name = ct.function_name ^ "_fin" in
      let then_label = str_of_instr_w (LABEL then_name) in
      let fin_label = str_of_instr_w (LABEL fin_name) in
      let jump_final = str_of_instr_w (J fin_name) in
      let condition = (generate_code cond) in
      
      if is_has_register_to_return() then begin
        let to_return = Stack.top ct.to_return_stack in
        let then_code = generate_code thn in
        let then_move_reg = str_of_instr_w (MV(to_return, ct.binop_returned)) in
        
        let else_code = generate_code els in
        let else_move_reg = str_of_instr_w (MV(to_return, ct.binop_returned)) in

        condition ^ else_code ^ else_move_reg ^ jump_final ^ then_label ^ then_code ^ then_move_reg ^ fin_label
      end else begin
        let then_code = generate_code thn in        
        let else_code = generate_code els in
        condition ^ else_code ^ jump_final ^ then_label ^ then_code ^ fin_label
      end
    end
| EBinop (op, left, right) ->
    let left_code = generate_code left in
    let rs1 = ct.binop_returned in
    let right_code = generate_code right in
    let rs2 = ct.binop_returned in
    free_register temp_regs rs1;
    free_register temp_regs rs2;
    let rd = get_free_register temp_regs in
    let inst = begin
      match op with
      | Add ->      str_of_instr_w (ADD(rd, rs1, rs2))
      | Sub ->      str_of_instr_w (SUB(rd, rs1, rs2))
      | Multiply -> str_of_instr_w (MUL(rd, rs1, rs2))
      | Divide ->   str_of_instr_w (DIV(rd, rs1, rs2))
    end in
    ct.binop_returned <- rd;
    left_code ^ right_code ^ inst;
| ESeqLocal(curr, next) -> generate_code curr ^ generate_code next
| ENum(num) -> 
    let rd = get_free_register temp_regs in
    ct.binop_returned <- rd;
    str_of_instr_w(LI(rd, num))
| EVar(name) ->
    let rd = begin
    match Hashtbl.find_opt reg_table name with
    | Some v -> v
    | None -> 
        let new_reg = get_free_register arg_regs in
        Hashtbl.add reg_table name new_reg;
        new_reg
    end in
    ct.binop_returned <- rd;
    "";
| ECond(left, op, right) -> 
    let left_code = generate_code left in
    let left_res = ct.binop_returned in
    let right_code = generate_code right in
    let right_res = ct.binop_returned in
    let label_name = ct.function_name ^ "_then" in
    left_code ^ right_code ^
    begin match op with 
    | TGe -> str_of_instr_w (BGE(left_res, right_res, label_name))
    | TGt -> str_of_instr_w (BGT(left_res, right_res, label_name))
    | TLt -> str_of_instr_w (BLT(left_res, right_res, label_name))
    | TLe -> str_of_instr_w (BLE(left_res, right_res, label_name))
    | TEq -> str_of_instr_w (BEQ(left_res, right_res, label_name))
    | TNe -> str_of_instr_w (BNE(left_res, right_res, label_name))
    | _ -> failwith "Expected one of '<=', '<', '>', '>=' in condition."
    end
| ELet(name, expr) -> 
    let code = generate_code expr in
    let rs = ct.binop_returned in
    code ^ str_of_instr_w (MV("skel", rs))
| ECall(name, exp) ->
    let code = generate_code exp in
    ct.has_callings <- true;
    let rd = ct.binop_returned in
    (* ct.binop_returned <- "a0"; (* Проблема в том, что если положить результат от вызова функции в a0, 
    то нынешний аргумент a0 перезатрётся. Поэтому сразу после получения перекладываем результат во временный регистр,
    а аргумент восстанавливаем со стека
      *) *)
    ct.binop_returned <- get_free_register temp_regs;
    (* let save_ra = str_of_instr_w(SD("ra", 0, "sp")) in *)
    let pos = ref 8 in

    (* let saved_regs = ref "" in *)
    let loaded_regs = ref "" in
    reg_table
    |> Hashtbl.to_seq
    |> Seq.iter (fun (_, value) -> 
      (* printf "Key: %s, value: %s\n" key value *)
      (* saved_regs := !saved_regs ^   str_of_instr_w(SD(value, !pos, "sp")); *)
      loaded_regs := !loaded_regs ^  str_of_instr_w(LD(value, !pos, "sp"));
      pos := !pos + 8;
    );
    let move_res_to_tempr = str_of_instr_w(MV(ct.binop_returned, "a0")) in
    let load_ra = str_of_instr_w(LD("ra",0, "sp")) in

    code ^ 
    str_of_instr_w (MV("a0", rd)) ^
    (* save_ra ^ !saved_regs ^ *)
    str_of_instr_w (CALL(name)) ^
    move_res_to_tempr ^
    load_ra ^ !loaded_regs
| _ -> failwith "Not implemented";;

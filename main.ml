open Printf

type oper = Add | Subtract | Multiply | Divide
type cmp_sign =
| GREATER_EQUAL
| LESS_EQUAL

(* Instructions of higher level IR *)
type action = 
| Sequence of action * action             (* current "action" and next "action" *)
| LetR of string * string * oper * string (* rd = rs1 op rs2 *)
| LetI of string * string * oper * int    (* rd = rs1 op imm *)
| Putarg of string                        (* put register data to a0 *)
| Condition of string * cmp_sign * int    (* rs sign imm, e.g. a0 >= 0 *)
| Return of string
| Call of string
| EndOfProgram
| If of action * action * action          (* condition, if-body, else-body *)
| While of action * action                (* condition, cycle body *)
| Function of string * string * action    (* function name, argument, function body *)

type compile_context = {
  function_name : string;
}
let start_context = {function_name = "_start"}

(* Supported instructions of RISC-V assembler *)
type instr = 
| LABEL of string
| MV of string * string
| LI of string * int
| CALL of string
| ADD of string * string * string
| SUB of string * string * string
| MUL of string * string * string
| DIV of string * string * string
| ADDI of string * string * int
| SD of string * int * string
| LD of string * int * string
| BGE of string * string * string (* branch if >= *)
| J of string
| RET
| ECALL

let str_of_instr = function
| LABEL(name) -> name ^ ":"
| MV(rd, rs) -> sprintf "mv %s, %s" rd rs
| LI(rd, imm) -> sprintf "li %s, %d" rd imm
| CALL(func) ->  sprintf "call %s" func
| ADD(rd, rs1, rs2) -> sprintf "add %s, %s, %s" rd rs1 rs2
| SUB(rd, rs1, rs2) -> sprintf "sub %s, %s, %s" rd rs1 rs2
| MUL(rd, rs1, rs2) -> sprintf "mul %s, %s, %s" rd rs1 rs2
| DIV(rd, rs1, rs2) -> sprintf "div %s, %s, %s" rd rs1 rs2
| ADDI(rd, rs, imm) -> sprintf "addi %s, %s, %d" rd rs imm
| SD(rs, shift, addr) -> sprintf "sd %s, %d(%s)" rs shift addr
| LD(rd, shift, addr) -> sprintf "ld %s, %d(%s)" rd shift addr
| BGE(rs1, rs2, label) -> sprintf "bge %s, %s, %s" rs1 rs2 label
| J(label) -> "j " ^ label
| RET -> "ret"
| ECALL -> "ecall";;
let str_of_instr_w v = 
  match v with
  | LABEL(_) -> str_of_instr v ^ "\n"
  | MV(rd, rs) -> if rd <> rs then "\t" ^ str_of_instr v ^ "\n" else ""
  | _ -> "\t" ^ str_of_instr v ^ "\n";;

let str_of_condition condition label =
  match condition with
  | (rs1, cmp_sign, imm) -> 
      let temp_inst = LI("t0", imm) in
      let next_inst = 
      match cmp_sign with
      | GREATER_EQUAL -> BGE(rs1, "t0", label)
      | LESS_EQUAL -> BGE("t0", rs1, label) in
      str_of_instr_w temp_inst ^ str_of_instr_w next_inst;;
    
let rec parse_program (context : compile_context) = function
| Function(func_name, arg, seq) -> 
    let new_context = {context with function_name=func_name} in
    let label = str_of_instr_w (LABEL func_name) in
    let st = str_of_instr_w (ADDI ("sp", "sp", -16)) in 
    let imm = parse_program new_context seq in
    label ^ st ^ imm
| Sequence(curr, next) ->
    let curr_str = parse_program context curr in
    let next_str = parse_program context next in
      curr_str ^ next_str
| If (condition, thn, els) ->
    begin  
    match condition with
    Condition (c_rs1, c_sign, c_imm) ->
      let then_name = context.function_name ^ "_then" in
      let fin_name = context.function_name ^ "_fin" in
      let then_label = str_of_instr_w (LABEL then_name) in
      let fin_label = str_of_instr_w (LABEL fin_name) in
      let cond = (c_rs1, c_sign, c_imm) in
      let cond_str = (str_of_condition cond then_name) in
      let thn_str = parse_program context thn in
      let els_str = parse_program context els in
      cond_str ^ els_str ^ then_label ^ thn_str ^ fin_label
    | _ -> failwith "ERROR: Expected condition in if-clause, but actual is not."
    end
| While(condition, body) -> 
    begin
      match condition with
      | Condition (c_rs1, c_sign, c_imm) ->
          let while_name_start = context.function_name ^ "_while_start" in
          let while_name_end = context.function_name ^ "_while_end" in
          let while_start_label  = str_of_instr_w(LABEL while_name_start) in
          let while_end_label = str_of_instr_w(LABEL while_name_end) in
          let jump_start_str = str_of_instr_w(J while_name_start) in
          let body_str = parse_program context body in
          let cond = (c_rs1, c_sign, c_imm) in
          let cond_str = (str_of_condition cond while_name_end) in
          while_start_label ^ cond_str ^ body_str ^ jump_start_str ^ while_end_label
      | _ -> failwith "ERROR: Expected condition in while-clause, but actual is not."
    end
| LetI(rd, rs1, op, imm) ->
    begin 
      match op with
      | Add ->      if rs1 = "x0" then str_of_instr_w (LI(rd, imm))
                                  else str_of_instr_w (ADDI(rd, rs1, imm))
      | Subtract -> str_of_instr_w (ADDI(rd, rs1, -imm))
      | Multiply -> str_of_instr_w (LI("t0", imm)) ^
                    str_of_instr_w (MUL(rd, rs1, "t0"))
      | Divide ->   str_of_instr_w (LI("t0", imm)) ^
                    str_of_instr_w (DIV(rd, rs1, "t0"))
    end
| LetR(rd, rs1, op, rs2) ->
    begin
      match op with
      | Add ->      if rs1 = "x0" then str_of_instr_w (MV(rd, rs2))
                    else if rs2 = "x0" then str_of_instr_w (MV(rd, rs1))
                    else str_of_instr_w (ADD(rd, rs1, rs2))
      | Subtract -> str_of_instr_w (SUB(rd, rs1, rs2))
      | Multiply -> str_of_instr_w (MUL(rd, rs1, rs2))
      | Divide ->   str_of_instr_w (DIV(rd, rs1, rs2))
    end
| Putarg(rs) -> str_of_instr_w (MV("a0", rs))
| Return(rs) -> parse_program context (Putarg rs) ^
                str_of_instr_w (ADDI("sp", "sp", 16)) ^
                str_of_instr_w (RET)
                (* str_of_instr_w (J (context.function_name ^ "_fin")) *)
| Call(name) -> 
    let save_callee =     str_of_instr_w (SD ("ra", 0, "sp")) in
    let call_str =        str_of_instr_w (CALL(name)) in
    let restore_callee =  str_of_instr_w (LD ("ra", 0, "sp")) in
    save_callee ^ call_str ^ restore_callee
| EndOfProgram -> str_of_instr_w (LI ("a7", 94)) ^
                  str_of_instr_w ECALL
| _ -> failwith "Error: Undefined action!";;

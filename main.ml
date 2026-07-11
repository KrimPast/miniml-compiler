type oper = Add | Subtract | Multiply | Divide
type atomic = 
| Const of int
| Register of string

type cmp_sign =
| GREATER_EQUAL
| LESS_EQUAL

type action =
(* | Binop of oper * atomic * atomic *)
| LetR of string * string * oper * string
| LetI of string * string * oper * int

| Putarg of string (* put register data to a0 *)
| Condition of string * cmp_sign * int
| Return of string
| Call of string
| Ecall

type nested = 
| If of action * nested * nested
| Sequence of action * nested
| Function of string * string * nested (* function name, argument, function body *)
| SingleNested of action

type compile_context = {
  function_name : string;
}
let start_context = {function_name = "_start"}

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

let str_of_instr = function
| LABEL(name) -> name ^ ":"
| MV(rd, rs) -> Printf.sprintf "mv %s, %s" rd rs
| LI(rd, imm) -> Printf.sprintf "li %s, %d" rd imm
| CALL(func) ->  Printf.sprintf "call %s" func
| ADD(rd, rs1, rs2) -> Printf.sprintf "add %s, %s, %s" rd rs1 rs2
| SUB(rd, rs1, rs2) -> Printf.sprintf "sub %s, %s, %s" rd rs1 rs2
| MUL(rd, rs1, rs2) -> Printf.sprintf "mul %s, %s, %s" rd rs1 rs2
| DIV(rd, rs1, rs2) -> Printf.sprintf "div %s, %s, %s" rd rs1 rs2
| ADDI(rd, rs, imm) -> Printf.sprintf "addi %s, %s, %d" rd rs imm
| SD(rs, shift, addr) -> Printf.sprintf "sd %s, %d(%s)" rs shift addr
| LD(rd, shift, addr) -> Printf.sprintf "ld %s, %d(%s)" rd shift addr
| BGE(rs1, rs2, label) -> Printf.sprintf "bge %s, %s, %s" rs1 rs2 label
| J(label) -> "j " ^ label
| RET -> "ret";;
let str_of_instr_w v = 
  match v with
  | LABEL(_) -> str_of_instr v ^ "\n"
  | MV(rd, rs) -> if rd <> rs then "\t" ^ str_of_instr v ^ "\n" else ""
  | _ -> "\t" ^ str_of_instr v ^ "\n";;

let rec str_of_action (context : compile_context) = function
| LetI(rd, rs1, op, imm) ->
    begin 
      match op with
      | Add ->      str_of_instr_w (ADDI(rd, rs1, imm))
      | Subtract -> str_of_instr_w (LI("t0", imm)) ^
                    str_of_instr_w (SUB(rd, rs1, "t0"))
      | Multiply -> str_of_instr_w (LI("t0", imm)) ^
                    str_of_instr_w (MUL(rd, rs1, "t0"))
      | Divide ->   str_of_instr_w (LI("t0", imm)) ^
                    str_of_instr_w (DIV(rd, rs1, "t0"))
    end
| LetR(rd, rs1, op, rs2) ->
    begin
      match op with
      | Add ->      str_of_instr_w (ADD(rd, rs1, rs2))
      | Subtract -> str_of_instr_w (SUB(rd, rs1, rs2))
                    
      | Multiply -> str_of_instr_w (MUL(rd, rs1, rs2))
      | Divide ->   str_of_instr_w (DIV(rd, rs1, rs2))
    end
| Putarg(rs) -> str_of_instr_w (MV("a0", rs))
| Return(rs) -> str_of_action context (Putarg rs) ^ 
                str_of_instr_w (J (context.function_name ^ "_fin"))
| Call(name) -> 
    let save_callee = str_of_instr_w (SD ("ra", 0, "sp")) in
    let call_str = str_of_instr_w (CALL(name)) in
    let restore_callee = str_of_instr_w (LD ("ra", 0, "sp")) in
    save_callee ^ call_str ^ restore_callee
| Ecall -> "\tecall\n"
| _ -> failwith "Error: Undefined action!";;

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
    let ed = str_of_instr_w (ADDI ("sp", "sp", 16)) in 
    let return = str_of_instr_w RET in
    let imm = parse_program new_context seq in
    label ^ st ^ imm ^ ed ^ return
| Sequence(curr, next_seq) ->
  let next_seq = parse_program context next_seq in
    str_of_action context curr ^ next_seq
| If (condition, thn, els) ->
    begin  
    match condition with
    Condition (c_rs1, c_sign, c_imm) ->
      let then_name = context.function_name ^ "_then" in
      let fin_name = context.function_name ^ "_fin" in
      let then_label = str_of_instr_w (LABEL then_name) in
      let fin_label = str_of_instr_w (LABEL fin_name) in
    (*let jump_fin = str_of_instr_w (J("then1")) in *)
      let cond = (c_rs1, c_sign, c_imm) in
      let cond_str = (str_of_condition cond then_name) in
      let thn_str = parse_program context thn in
      let els_str = parse_program context els in
      cond_str ^ els_str (*^ jump_fin *) ^ then_label ^ thn_str ^ fin_label
    | _ -> failwith "ERROR: Expected condition in if-clause, but actual is not."
    end
| SingleNested(action) -> str_of_action context action;;


let start = 

Function("_start", "a0",
  Sequence(
    LetI("a0", "x0", Add, 4),
    Sequence(
      Call "factorial",
      Sequence(
        LetI("a7", "x0", Add, 93),
        SingleNested(Ecall)
      ) 
    )
  )
)
let program = 

Function("factorial", "a0",
  If(Condition("a0", LESS_EQUAL, 1),
    Sequence(LetI("a0", "x0", Add, 1),
             SingleNested(Return "a0")),
    Sequence(LetI("a1", "a0", Add, -1),
      Sequence(Putarg "a1",
        Sequence(Call "factorial",
          Sequence(LetI("a1", "a1", Add, 1),
            Sequence(
              LetR("a0", "a0", Multiply, "a1"),
              SingleNested(Return "a0")
            )
          )
        )
      )
    )
  )
);;

print_string (parse_program start_context start ^ parse_program start_context program);;

let a () =
  print_string (str_of_instr_w (LABEL("fact")));
  print_string (str_of_instr_w (MV("a0", "a1")));
  print_string (str_of_instr_w (LI("a0", 5)));
  print_string (str_of_instr_w (CALL("fact")));
  print_string (str_of_instr_w (ADD("a0", "a1", "a2")));
  print_string (str_of_instr_w (SUB("a0", "a1", "a2")));
  print_string (str_of_instr_w (MUL("a0", "a1", "a2")));
  print_string (str_of_instr_w (ADDI("a0", "a1", 5)));
  print_string (str_of_instr_w (SD("ra", 8, "sp")));
  print_string (str_of_instr_w (LD("ra", 8, "sp")));
  print_string (str_of_instr_w RET);;
(* let () = a(); *)
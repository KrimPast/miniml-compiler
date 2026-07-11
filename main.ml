type oper = Add | Multiply | Divide
type atomic = 
| Const of int
| Register of string

type cmp_sign =
| GREATER_EQUAL
| LESS_EQUAL

type action =
| Binop of oper * atomic * atomic
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


(* let example = Binop(Add, Const 5, Binop(Multiply, Const 5, Const 6))  *)

(* let var = Let("my_var", 15);; *)

(* let print_var (v : expr) =
  match v with
  | Let(str, num) -> 
      print_endline (str ^ ": " ^ string_of_int num)
  | _ -> failwith "Error";; *)


(*print_var var;;*)

type instr = 
| LABEL of string
| MV of string * string
| LI of string * int
| CALL of string
| ADD of string * string * string
| SUB of string * string * string
| MUL of string * string * string
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

let rec str_of_action = function
| LetI(rd, rs1, op, imm) ->
    begin 
      match op with
      | Add -> str_of_instr_w(ADDI(rd, rs1, imm))
    end
| LetR(rd, rs1, op, rs2) ->
    begin
      match op with
      | Add -> str_of_instr_w (ADD(rd, rs1, rs2))
      | Multiply -> str_of_instr_w (MUL(rd, rs1, rs2))
    end
| Putarg(rs) -> str_of_instr_w (MV("a0", rs))
| Return(rs) -> str_of_action (Putarg rs) ^ str_of_instr_w (J "fin1")
| Call(name) -> 
    let save_callee = str_of_instr_w (SD ("ra", 0, "sp")) in
    let call_str = str_of_instr_w (CALL(name)) in
    let restore_callee = str_of_instr_w (LD ("ra", 0, "sp")) in
    save_callee ^ call_str ^ restore_callee
| Ecall -> "\tecall\n";;

let str_of_condition condition label =
  match condition with
  | (rs1, cmp_sign, imm) -> 
      let temp_inst = LI("t0", imm) in
      let next_inst = 
      match cmp_sign with
      | GREATER_EQUAL -> BGE(rs1, "t0", label)
      | LESS_EQUAL -> BGE("t0", rs1, label) in
      str_of_instr_w temp_inst ^ str_of_instr_w next_inst;;
    
let rec parse_program = function
| Function(func_name, arg, seq) -> 
    let label = str_of_instr_w (LABEL func_name) in
    let st =  str_of_instr_w (ADDI ("sp", "sp", -16)) (* ^ 
              str_of_instr_w (SD (arg, 8, "sp")) ^
              str_of_instr_w (SD ("ra", 0, "sp")) *) in 
    let ed =  str_of_instr_w (ADDI ("sp", "sp", 16)) (* ^ 
              str_of_instr_w (LD (arg, 8, "sp")) ^
              str_of_instr_w (LD ("ra", 0, "sp")) *) in 
    let return = str_of_instr_w RET in
    let imm = parse_program seq in
    label ^ st ^ imm ^ ed ^ return
| Sequence(curr, next_seq) ->
  let next_seq = parse_program next_seq in
    str_of_action curr ^ next_seq
| If (condition, thn, els) ->
    begin  
    match condition with
    Condition (c_rs1, c_sign, c_imm) ->
      let then_label = str_of_instr_w (LABEL("then1")) in
      let fin_label = str_of_instr_w (LABEL("fin1")) in
    (*let jump_fin = str_of_instr_w (J("then1")) in *)
      let cond = (c_rs1, c_sign, c_imm) in
      let cond_str = (str_of_condition cond "then1") in
      let thn_str = parse_program thn in
      let els_str = parse_program els in
      cond_str ^ els_str (*^ jump_fin *) ^ then_label ^ thn_str ^ fin_label
    | _ -> failwith "ERROR: Expected condition in if-clause, but actual is not."
    end
| SingleNested(action) -> str_of_action action;;


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

print_string (parse_program start ^ parse_program program);;

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
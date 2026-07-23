(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Printf

(** Supported instructions of RISC-V assembler **)
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
  | BGE of string * string * string (* >= *)
  | BGT of string * string * string (* > *)
  | BLT of string * string * string (* < *)
  | BLE of string * string * string (* <= *)
  | BEQ of string * string * string (* = *)
  | BNE of string * string * string (* <> *)
  | LA of string * string (* load address *)
  | J of string
  | RET
  | ECALL

let str_of_instr = function
  | LABEL name -> name ^ ":"
  | MV (rd, rs) -> sprintf "mv %s, %s" rd rs
  | LI (rd, imm) -> sprintf "li %s, %d" rd imm
  | CALL func -> sprintf "call %s" func
  | ADD (rd, rs1, rs2) -> sprintf "add %s, %s, %s" rd rs1 rs2
  | SUB (rd, rs1, rs2) -> sprintf "sub %s, %s, %s" rd rs1 rs2
  | MUL (rd, rs1, rs2) -> sprintf "mul %s, %s, %s" rd rs1 rs2
  | DIV (rd, rs1, rs2) -> sprintf "div %s, %s, %s" rd rs1 rs2
  | ADDI (rd, rs, imm) -> sprintf "addi %s, %s, %d" rd rs imm
  | SD (rs, shift, addr) -> sprintf "sd %s, %d(%s)" rs shift addr
  | LD (rd, shift, addr) -> sprintf "ld %s, %d(%s)" rd shift addr
  | BGE (rs1, rs2, label) -> sprintf "bge %s, %s, %s" rs1 rs2 label
  | BGT (rs1, rs2, label) -> sprintf "bgt %s, %s, %s" rs1 rs2 label
  | BLT (rs1, rs2, label) -> sprintf "blt %s, %s, %s" rs1 rs2 label
  | BLE (rs1, rs2, label) -> sprintf "ble %s, %s, %s" rs1 rs2 label
  | BEQ (rs1, rs2, label) -> sprintf "beq %s, %s, %s" rs1 rs2 label
  | BNE (rs1, rs2, label) -> sprintf "bne %s, %s, %s" rs1 rs2 label
  | LA (rs, label) -> sprintf "la %s, %s" rs label
  | J label -> "j " ^ label
  | RET -> "ret"
  | ECALL -> "ecall"

let str_of_instr_w v =
  match v with
  | LABEL _ -> str_of_instr v ^ "\n"
  | MV (rd, rs) -> if rd <> rs then "\t" ^ str_of_instr v ^ "\n" else ""
  | _ -> "\t" ^ str_of_instr v ^ "\n"

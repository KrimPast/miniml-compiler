(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

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

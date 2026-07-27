(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Tokens

type op = Add | Sub | Multiply | Divide

type expr =
  | ENum of int
  | EVar of string
  | EBinop of op * expr * expr
  | EIf of expr * expr * expr
  | ECond of expr * token * expr
  | ESeq of expr * expr
  | ESeqLocal of expr * expr
  | EFunc of { name : string; args : string list; body : expr; is_rec : bool }
  | EClosureAlloc of string (* func name *)
  | EClosureApply of string * expr (* func name and arg *)
  | ELet of string * expr
  | ECall of string * expr list (* func name and args *)
  | ENothing

type splacement =
  | SPlaceIsReg of string
  | SPlaceIsStack of { register : string; offset : int }

type sexpr =
  | SFunc of { amount_args : int }
  | SVar of { placement : splacement }
  | SClosure of { placement : splacement; remaining_args : int }

type call_type = FullCall | NewClosure | OldClosure
type stack_action = StoreRegs | LoadRegs

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Multiply -> "*"
  | Divide -> "/"

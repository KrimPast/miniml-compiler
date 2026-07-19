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
  | EFunc of string * string * expr (* func name, argument, body*)
  | ELet of string * expr
  | ECall of string * expr (* func name and argument *)
  | ENothing

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Multiply -> "*"
  | Divide -> "/"

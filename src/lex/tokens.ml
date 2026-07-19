(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

type token =
  | TIf
  | TThen
  | TElse
  | TRec
  | TLet
  | TNum of int
  | TID of string
  | TEq
  | TNe
  | TPlus
  | TMinus
  | TMul
  | TDiv
  | TLe
  | TGe
  | TLt
  | TGt
  | TLParen
  | TRParen
  | TContinueLocal
  | TSeq
  | TSeqEnd
  | TEnd

exception LexError of string

let string_of_token = function
  | TIf -> "if"
  | TThen -> "then"
  | TElse -> "else"
  | TLet -> "let"
  | TContinueLocal -> "in"
  | TSeq -> ";"
  | TSeqEnd -> ";;"
  | TNum n -> "TNum(" ^ string_of_int n ^ ")"
  | TID name -> "TID(" ^ name ^ ")"
  | TRec -> "rec"
  | TLe -> "<="
  | TGe -> ">="
  | TLt -> "<"
  | TGt -> ">"
  | TEq -> "="
  | TNe -> "<>"
  | TPlus -> "+"
  | TMinus -> "-"
  | TMul -> "*"
  | TDiv -> "/"
  | TLParen -> "("
  | TRParen -> ")"
  | TEnd -> "$"

let token_of_string = function
  | "if" -> TIf
  | "then" -> TThen
  | "else" -> TElse
  | "let" -> TLet
  | "=" -> TEq
  | "+" -> TPlus
  | "-" -> TMinus
  | "*" -> TMul
  | "/" -> TDiv
  | "(" -> TLParen
  | ")" -> TRParen
  | "$" -> TEnd
  | _ -> failwith "ERROR!"

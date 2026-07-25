(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

{
  open Printf
  open Tokens
}
let digit = ['0'-'9']
let space = [' ' '\t' '\n']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*

rule token = parse
  | space+      { token lexbuf }
  | digit+ as num
                { 
                  TNum (int_of_string num)
                }
  | "let"       { TLet }
  | "if"        { TIf }
  | "then"      { TThen }
  | "else"      { TElse }
  | "rec"       { TRec }
  | "in"        { TContinueLocal }
  | id as text  { 
                  TID(text)
                }
  | "<="        { TLe }
  | "<"         { TLt }
  | ">"         { TGt }
  | ">="        { TGe }

  | '='         { TEq }
  | "<>"        { TNe }
  | '+'         { TPlus }
  | '-'         { TMinus }
  | '*'         { TMul }
  | '/'         { TDiv }
  | '('         { TLParen }
  | ')'         { TRParen }
  | ";;"        { TSeqEnd }
  | ";"         { TSeq }

  | eof         { TEnd }
  | _           {
                  raise (LexError( sprintf "Undefined symbol: '%s'" (Lexing.lexeme lexbuf)))
                }
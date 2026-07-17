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
                  (* printf "integer: %s (%d)\n" num (int_of_string num); *)
                  TNum (int_of_string num)
                }
  | "let"       { TLet }
  | "if"        { TIf }
  | "then"      { TThen }
  | "else"      { TElse }
  | "rec"       { TRec }
  | id as text  { 
                  (* printf "var: %s\n" text; *)
                  TID(text)
                }
  | "<="        { TLq }
  | '='         { TEq }
  | '+'         { TPlus }
  | '-'         { TMinus }
  | '*'         { TMul }
  | '/'         { TDiv }
  | '('         { TLParen }
  | ')'         { TRParen }
  | eof         { TEnd }
  | _           {
                  let word = (Lexing.lexeme lexbuf) in
                  let position = (Lexing.lexeme_start_p lexbuf) in
                  let line = position.pos_lnum in
                  let sym = position.pos_bol in
                  raise (LexError( sprintf "Undefined symbol(line %d, sym %d) '%s'" line sym word))
                }
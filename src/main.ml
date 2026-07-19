(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Lex
open Gn.Gen
let read_file filename =
  let file = open_in filename in
  let source = really_input_string file (in_channel_length file) in
  close_in file;
  source;;

type helpTokens =
| ShowAll
| ShowTokenized
| ShowAssembler
| ShowHelp
| Undefined

let check_if_token = function
| "-a" | "--all" -> ShowAll
| "-t" | "--show-tokenized" -> ShowTokenized
| "-S" | "--show-assembler" -> ShowAssembler
| "-h" | "--help" -> ShowHelp
| _ -> Undefined

let print_short_help () = 
{|compiler.exe: unbound arguments.
To show help use "--help" flag. |} |> print_endline
let print_help () = 
{|NAME
    compiler.exe - simple miniML compiler

SYNOPSIS
    compiler.exe infile.ml [-a|-t|-S]

OPTIONS
    -a, --all
        Output all of intermediate stages of compiling
    -t, --show-tokenized
        Output tokens resulting from lexer parsing
    -S, --show-assembler
        Output resulting assembler code of program

    -h, --help
        Show this help|} |> print_endline

type compile_stages = {
  source : string;
  tokens : Tokens.token array;
  exp : Gn.Exprs.expr;
  code : string;
}
let compile_program file =
  let source = read_file file in
  let tokens = lex_ocamllex source in
  let exp = Parser.parse tokens in
  let code = generate_code exp in

  if List.length !temp_regs <> 8 then
    print_endline (Printf.sprintf "warning: Not all regs were freed!(%d/%d)" (List.length !temp_regs) 8);
  
    {source; tokens; exp; code}

let () =
  let amount_args = Array.length Sys.argv in
  
  if amount_args = 2 then begin
    let some = check_if_token Sys.argv.(1) in
    match some with
    | ShowHelp -> print_help()
    | _ -> 
        let output = compile_program Sys.argv.(1) in
        print_endline output.code;
  end
  else if amount_args = 3 then  
  begin
    let flag = check_if_token Sys.argv.(2) in

    let output = compile_program Sys.argv.(1) in
    match flag with
    | ShowAll ->
        print_endline "Program:";
        print_endline @@ output.source ^ "\n";

        print_endline "Tokens:";
        print_tokens_list output.tokens;
        print_endline "\n";
        
        print_endline "Code: ";
        print_endline output.code;
    | ShowAssembler ->
        print_endline output.code;
    | ShowTokenized ->
        print_tokens_list output.tokens;
        print_endline "\n";
    | ShowHelp ->
        print_help()
    | Undefined ->
        print_short_help()
  end 
  else print_short_help() 
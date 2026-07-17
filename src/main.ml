(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Lex
open Gn.Gen
let read_file filename =
  let file = open_in filename in
  let source = really_input_string file (in_channel_length file) in
  close_in file;
  source

let () =
  if Array.length Sys.argv = 2 then begin
      let source = read_file Sys.argv.(1) in
      print_endline "Program:";
      print_endline @@ source ^ "\n";

      let tokens = lex_ocamllex source in
      print_endline "Tokens:";
      print_tokens_list tokens;

      print_endline "\n";
      let exp = Parser.parse tokens in
      let code = generate_code exp in

      print_endline "Code: ";
      print_endline code;
    end
  else 
    failwith "[err]: Enter file to compile";


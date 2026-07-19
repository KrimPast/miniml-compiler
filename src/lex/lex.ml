(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Tokens

let lex_ocamllex s =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    match Lexer.token lexbuf with
    | TEnd -> Array.of_list @@ List.rev (TEnd :: acc)
    | tok -> loop (tok :: acc)
  in
  loop []

let print_token t = t |> string_of_token |> print_string

let print_tokens_list lt =
  let i = ref 0 in
  let k = Array.length lt in
  print_endline ("Length of tokens array: " ^ string_of_int k);
  while !i < k do
    print_token lt.(!i);
    print_string " ";
    i := !i + 1
  done

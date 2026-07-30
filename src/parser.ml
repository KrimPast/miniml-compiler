(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov *)

open Tokens
open Printf
open Gn.Exprs

exception ParseError of string

type parser_context = { mutable is_ecall_check : bool }

let parse tokens =
  let tok = ref 0 in
  let curr_token () = tokens.(!tok) in
  let to_next_token () = tok := !tok + 1 in
  let is_end tk = tk >= Array.length tokens in
  let eat (tk : token) =
    let real_token = curr_token () in
    to_next_token ();
    if tk <> real_token then
      raise
        (ParseError
           (sprintf "Expected <%s>, but got <%s>." (string_of_token tk)
              (string_of_token real_token)))
  in
  let ct = { is_ecall_check = false } in
  let rec e () =
    match curr_token () with
    | TLet ->
        eat TLet;
        let is_rec = ref false in
        let is_func = ref false in
        (* скипаем keyword "rec" *)
        (match curr_token () with
        | TRec ->
            is_rec := true;
            is_func := true;
            to_next_token ()
        | _ -> ());
        let name =
          match curr_token () with
          | TID x -> x
          | other ->
              raise
                (ParseError
                   (sprintf "Expected let name, got '%s'"
                      (string_of_token other)))
        in
        to_next_token ();
        let args = ref [] in
        while match curr_token () with TID _ -> true | _ -> false do
          args := !args @ [ curr_token () ];
          is_func := true;
          to_next_token ()
        done;
        eat TEq;
        let body = e () in
        if !is_func = true then
          let args_str = List.map string_of_token_clear !args in
          let func = EFunc { name; args = args_str; body; is_rec = !is_rec } in
          if curr_token () <> TEnd then (
            if curr_token () = TSeqEnd then eat TSeqEnd;
            ESeq (func, e ()))
          else func
        else if curr_token () = TContinueLocal then (
          eat TContinueLocal;
          ESeqLocal (ELet (name, body), e ()))
        else ELet (name, body)
    | TIf -> (
        eat TIf;
        let left = e () in
        let sign = curr_token () in
        to_next_token ();
        let right = e () in
        eat TThen;
        let thn = e () in
        match curr_token () with
        | TElse ->
            to_next_token ();
            let els = e () in
            EIf (ECond (left, sign, right), thn, els)
        | _ -> EIf (ECond (left, sign, right), thn, ENothing))
    | TID _ | TNum _ | TLParen ->
        let left = t () in
        e' left
    | TEnd -> ENothing
    | other ->
        raise
          (ParseError (sprintf "Unexpected token '%s'" (string_of_token other)))
  and e' left =
    match curr_token () with
    | TPlus ->
        eat TPlus;
        let right = t () in
        let new_left = EBinop (Add, left, right) in
        e' new_left
    | TMinus ->
        eat TMinus;
        let right = t () in
        let new_left = EBinop (Sub, left, right) in
        e' new_left
    | TEnd | TRParen | TEq -> left
    | _ -> left
  and t () =
    match curr_token () with
    | TID _ | TNum _ | TLParen ->
        let left = f () in
        t' left
    | other ->
        raise
          (ParseError (sprintf "Unexpected token '%s'" (string_of_token other)))
  and t' left =
    match curr_token () with
    | TPlus | TMinus -> left
    | TMul ->
        eat TMul;
        let right = f () in
        let new_left = EBinop (Multiply, left, right) in
        t' new_left
    | TDiv ->
        eat TDiv;
        let right = f () in
        let new_left = EBinop (Divide, left, right) in
        t' new_left
    | TEnd | TRParen | TEq -> left
    | _ -> left
  and f () =
    match curr_token () with
    | TID name ->
        eat (TID name);
        if ct.is_ecall_check then EVar name
        else (
          ct.is_ecall_check <- true;
          let args = ref [] in
          while
            (not (is_end !tok))
            &&
            match curr_token () with
            | TNum _ | TID _ | TLParen -> true
            | _ -> false
          do
            args := !args @ [ e () ]
          done;
          ct.is_ecall_check <- false;
          if not (List.is_empty !args) then ECall (name, !args) else EVar name)
    | TNum n ->
        eat (TNum n);
        ENum n
    | TLParen ->
        eat TLParen;
        let left = e () in
        eat TRParen;
        left
    | other ->
        raise
          (ParseError (sprintf "Unexpected token '%s'" (string_of_token other)))
  in
  e ()

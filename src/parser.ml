(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

open Tokens
open Printf
open Gn.Gen
open Gn.Exprs

let parse tokens = 
begin
  let tok = ref 0 in

  let next_token () =
    tok := !tok + 1 in
  
  (* let print_token t =
    t |>
    string_of_token |>
    print_string; *)

  let eat (tk : token) = 
    let real_token = tokens.(!tok) in
    next_token ();
    if tk <> real_token then
        print_endline (sprintf "Expected <%s>, but got <%s>." (string_of_token tk) (string_of_token real_token)) in

  let rec e () =
    match tokens.(!tok) with
    | TLet ->
        eat(TLet);
        
        let is_func = ref false in
        (* скипаем keyword "rec" *)
        begin match tokens.(!tok) with TRec -> is_func := true; next_token() |  _ -> () end;

        let name = match tokens.(!tok) with TID(x) -> x | _ -> failwith "" in
        next_token ();

        (* print_endline @@ string_of_token tokens.(!tok); *)
        while begin (* скипаем аргументы *)
            match tokens.(!tok) with
            | TID(_) -> true
            | _ -> false
          end; do
          is_func := true;
          next_token() 
        done;

        eat(TEq);
        let body = e() in
        
        if !is_func = true then
          EFunc(name, body)
        else
          ELet(name, body)
    
    | TIf ->
        eat(TIf);
        let left = e() in
        
        let sign = tokens.(!tok) in
        next_token();

        let right = e() in
        eat(TThen);
        let thn = e () in

        begin
        match tokens.(!tok) with
          | TElse ->
            next_token();
            let els = e() in
            EIf(ECond(left, sign, right), thn, els)
          | _ -> 
            EIf(ECond(left, sign, right), thn, ENothing)
        end
    | TID(_) | TNum(_) | TLParen -> 
        let left = t () in
        e' left
    | other -> failwith @@ sprintf "Undefined token '%s'" (string_of_token other)
  and e' left =
    match tokens.(!tok) with
    | TPlus -> 
        eat(TPlus); 
        let right = t () in
        let new_left = EBinop(Add, left, right) in
        e' new_left
    | TMinus -> 
        eat(TMinus);
        let right = t () in
        let new_left = EBinop(Sub, left, right) in
        e' new_left
    | TEnd | TRParen | TEq  -> left
    | _ -> left
  and t () =
    match tokens.(!tok) with
    | TID(_) | TNum(_) | TLParen -> 
        let left = f () in 
        t' left
    | _ -> failwith "t"
  and t' left =
    match tokens.(!tok) with
    | TPlus | TMinus -> left
    | TMul -> 
        eat(TMul);
        let right = f () in
        let new_left = EBinop(Multiply, left, right) in
        t' new_left
    | TDiv ->  
        eat(TDiv); 
        let right = f () in
        let new_left = EBinop(Divide, left, right) in
        t' new_left
    | TEnd | TRParen | TEq -> left
    | _ -> left
  and f () =
    match tokens.(!tok) with
    | TID(x) -> 
        if !tok + 1 < Array.length tokens then begin
            (* print_string "Next token: ";  *)
            (* print_token tokens.(!tok + 1); *)
            (* print_endline ""; *)
            match tokens.(!tok + 1) with
            | TNum(_) | TID(_) | TLParen -> 
                next_token();
                let y = e() in
                (* print_endline "Съеден!"; *)
                ECall(x, y)
            | _ -> eat(TID(x)); EVar(x)
        end
        else begin 
          eat(TID(x)); 
          EVar(x)
        end
    | TNum(n) -> eat(TNum(n)); ENum(n)
    | TLParen -> 
        eat(TLParen); 
        let left = e () in
        eat(TRParen);
        left
    | _ -> failwith "f" in
  e()
end

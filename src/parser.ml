type token =
  | LetTok
  | EqualTok
  | NameTok of string
  | IntTok of int
  | EndTok

type expr =
  | Int of int
  | Var of string
  | Call of string * expr

type program =
  | Main of expr

exception Parse_error of string

(* Лексер. Новые ключевые слова и знаки добавлять здесь. *)
let tokenize source =
  let source =
    String.map
      (function '\n' | '\t' | '\r' -> ' ' | c -> c)
      source
  in

  let words =
    String.split_on_char ' ' source
    |> List.filter (fun word -> word <> "")
  in

  let token_of_word word =
    match word with
    | "let" -> LetTok
    | "=" -> EqualTok
    | _ ->
        match int_of_string_opt word with
        | Some value -> IntTok value
        | None -> NameTok word
  in

  List.map token_of_word words @ [EndTok]

type parser = {
  mutable tokens : token list;
}

let take parser =
  match parser.tokens with
  | token :: rest ->
      parser.tokens <- rest;
      token
  | [] ->
      EndTok

let peek parser =
  match parser.tokens with
  | token :: _ -> token
  | [] -> EndTok

let expect parser expected =
  if take parser <> expected then
    raise (Parse_error "Неожиданный токен")

(* Новые простые выражения добавлять здесь. *)
let parse_atom parser =
  match take parser with
  | IntTok value -> Int value
  | NameTok name -> Var name
  | _ -> raise (Parse_error "Ожидалось число или имя")

(* Сейчас: число, переменная или вызов функции с одним аргументом. *)
let parse_expr parser =
  let first = parse_atom parser in

  match first, peek parser with
  | Var function_name, (IntTok _ | NameTok _) ->
      Call (function_name, parse_atom parser)
  | _ ->
      first

(* Сейчас программа имеет форму: let main = выражение *)
let parse_program source =
  let parser = { tokens = tokenize source } in

  expect parser LetTok;

  begin
    match take parser with
    | NameTok "main" -> ()
    | _ -> raise (Parse_error "Ожидалось main")
  end;

  expect parser EqualTok;

  let expression = parse_expr parser in
  expect parser EndTok;

  Main expression

let rec show_expr = function
  | Int value -> Printf.sprintf "Int(%d)" value
  | Var name -> Printf.sprintf "Var(%s)" name
  | Call (name, argument) ->
      Printf.sprintf "Call(%s, %s)" name (show_expr argument)

let show_program = function
  | Main expression ->
      Printf.sprintf "Main(%s)" (show_expr expression)

let read_file filename =
  let file = open_in filename in
  let source = really_input_string file (in_channel_length file) in
  close_in file;
  source

let () =
  if Array.length Sys.argv <> 2 then
    prerr_endline "Запуск: parser.exe test.mml"
  else
    try
      let source = read_file Sys.argv.(1) in
      print_endline (show_program (parse_program source))
    with
    | Parse_error message ->
        prerr_endline ("Ошибка: " ^ message)

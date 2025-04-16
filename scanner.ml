open Token
module StringMap = Map.Make (String)

module Scanner = struct
  let ( >>= ) = Result.bind

  type state = {
    source : string;
    start : int;
    current : int;
    line : int;
    tokens : Token.token list;
  }

  type scan_error =
    | Unterminated_string of state
    | Invalid_character of char * state
    | Unexpected_eof of state
    | Malformed_number of string * state

  type 'a scanner_result = ('a, scan_error) result

  let scan_error_to_string = function
    | Unterminated_string state -> Printf.sprintf "Unterminated string at line %d" state.line
    | Invalid_character (ch, state) -> Printf.sprintf "Invalid character '%c' at line %d" ch state.line
    | Unexpected_eof state -> Printf.sprintf "Unexpected end of file at line %d" state.line
    | Malformed_number (num, state) -> Printf.sprintf "Malformed number '%s' at line %d" num state.line

  let keywords =
    StringMap.empty
    |> StringMap.add "and" Token.And
    |> StringMap.add "class" Token.Class
    |> StringMap.add "else" Token.Else
    |> StringMap.add "fun" Token.Fun
    |> StringMap.add "for" Token.For
    |> StringMap.add "if" Token.If
    |> StringMap.add "nil" Token.Nil
    |> StringMap.add "or" Token.Or
    |> StringMap.add "print" Token.Print
    |> StringMap.add "return" Token.Return
    |> StringMap.add "super" Token.Super
    |> StringMap.add "this" Token.This
    |> StringMap.add "var" Token.Var
    |> StringMap.add "while" Token.While

  let default_state source = { source; start = 0; current = 0; line = 1; tokens = [] }
  let is_at_end state = state.current >= String.length state.source
  let advance_current state = { state with current = state.current + 1 }
  let next_char state : char = String.get state.source (state.current + 1)
  let peek state : char option = if is_at_end state then None else Some (String.get state.source state.current)
  let peek_next state : char option = state |> advance_current |> peek
  let advance state = (peek state, advance_current state)
  let increment_line state = { state with line = state.line + 1 }
  let advance_start state = { state with start = state.current }
  let current_literal state = String.sub state.source state.start (state.current - state.start)
  let is_digit char = char >= '0' && char <= '9'

  let is_alpha ch =
    match ch with
    | 'a' .. 'z'
    | 'A' .. 'Z' ->
        true
    | _ -> false

  let is_alphanumeric ch = is_digit ch || is_alpha ch
  let extract_current_literal state = String.sub state.source (state.start + 1) (state.current - 1)

  let match_character expected_char state =
    match peek state with
    | Some ch when expected_char = ch -> (true, advance_current state)
    | _ -> (false, state)

  let add_token token_type lexeme literal line state =
    let token = { Token.lexeme; line; token_type; literal } in
    { state with tokens = state.tokens @ [ token ] }

  let rec consume_while predicate state =
    match peek state with
    | Some ch when predicate ch -> consume_while predicate (advance_current state)
    | _ -> state

  let consume_line = consume_while (fun ch -> ch <> '\n')

  let add_literal_token token_type literal_type state =
    let text = String.sub state.source state.start (state.current - state.start) in
    add_token token_type text literal_type state.line state

  let add_non_literal_token token_type state = add_literal_token token_type None state

  let add_conditional_non_literal_token expected_char conditional_token_type fallback_token_type state =
    match match_character expected_char state with
    | true, advanced -> add_non_literal_token conditional_token_type advanced
    | false, _ -> add_non_literal_token fallback_token_type state

  let add_identifier state =
    let consumed = consume_while is_alphanumeric state in
    let substring = current_literal consumed in
    match substring with
    | "true" -> add_literal_token Token.Bool (Some (Token.LBool true)) consumed
    | "false" -> add_literal_token Token.Bool (Some (Token.LBool false)) consumed
    | _ -> (
        match StringMap.find_opt substring keywords with
        | Some keyword -> add_non_literal_token keyword consumed
        | None -> add_non_literal_token Token.Identifier consumed)

  let add_number_literal state =
    let consume_if_fraction state =
      match (peek state, peek_next state) with
      | Some '.', Some char when is_digit char -> state |> advance_current |> consume_while is_digit
      | _ -> state
    in
    let add_number_token state =
      let str = current_literal state in
      add_literal_token Token.Number (Some (Token.LFloat (float_of_string str))) state
    in
    state |> consume_while is_digit |> consume_if_fraction |> add_number_token

  let add_string_literal state =
    let rec consume_until_string_end state =
      match peek state with
      | Some ch when ch <> '"' ->
          if ch = '\n' then state |> increment_line |> advance_current
          else state |> advance_current |> consume_until_string_end
      | _ -> state
    in
    let extract_current_literal state = String.sub state.source (state.start + 1) (state.current - state.start - 2) in
    let add_string_token state =
      let str = extract_current_literal state in
      add_literal_token Token.String (Some (Token.LString str)) state
    in
    let state_after_consuming = consume_until_string_end state in
    if is_at_end state_after_consuming then Error (Unterminated_string state)
    else
      let final_state = state_after_consuming |> advance_current |> add_string_token in
      Ok final_state

  let scan_token state : state scanner_result =
    if is_at_end state then Ok state
    else
      let ch, advanced = advance state in
      match ch with
      | Some '(' -> Ok (add_non_literal_token Token.Left_paren advanced)
      | Some ')' -> Ok (add_non_literal_token Token.Right_paren advanced)
      | Some '{' -> Ok (add_non_literal_token Token.Left_brace advanced)
      | Some '}' -> Ok (add_non_literal_token Token.Right_brace advanced)
      | Some ',' -> Ok (add_non_literal_token Token.Comma advanced)
      | Some '.' -> Ok (add_non_literal_token Token.Dot advanced)
      | Some '-' -> Ok (add_non_literal_token Token.Minus advanced)
      | Some '+' -> Ok (add_non_literal_token Token.Plus advanced)
      | Some ';' -> Ok (add_non_literal_token Token.Semicolon advanced)
      | Some '*' -> Ok (add_non_literal_token Token.Star advanced)
      | Some '!' -> Ok (add_conditional_non_literal_token '=' Token.Bang_equal Token.Bang advanced)
      | Some '=' -> Ok (add_conditional_non_literal_token '=' Token.Equal_equal Token.Equal advanced)
      | Some '<' -> Ok (add_conditional_non_literal_token '=' Token.Less_equal Token.Less advanced)
      | Some '>' -> Ok (add_conditional_non_literal_token '=' Token.Greater_equal Token.Greater advanced)
      | Some '/' -> (
          match match_character '/' advanced with
          | true, advanced -> Ok (consume_line advanced)
          | false, _ -> Ok (add_non_literal_token Token.Slash advanced))
      | Some '"' -> add_string_literal advanced
      | Some (' ' | '\r' | '\t') -> Ok advanced
      | Some '\n' -> Ok (increment_line advanced)
      | Some ch when is_digit ch -> Ok (add_number_literal advanced)
      | Some ch when is_alpha ch -> Ok (add_identifier advanced)
      | Some ch -> Error (Invalid_character (ch, advanced))
      | None -> Error (Unexpected_eof advanced)

  let scan_tokens source =
    let rec scan state =
      if is_at_end state then Ok (add_token Token.Eof "" None state.line state)
      else state |> advance_start |> scan_token >>= scan
    in
    default_state source |> scan >>= fun state -> Ok state.tokens
end

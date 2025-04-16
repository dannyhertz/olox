open Ast
open Token
open Utils

let ( >>= ) = Result.bind
let ( >>| ) r f = Result.map f r

module Parser = struct
  type state = {
    current : int;
    tokens : Token.token list;
  }

  type parse_error = Unexpected_token of Token.token * string

  let format_parse_error = function
    | Unexpected_token (token, message) -> Printf.sprintf "%d at '%s' %s" token.line token.lexeme message

  let make_state tokens = { current = 0; tokens }
  let advance_current state = { state with current = state.current + 1 }
  let previous state = List.nth state.tokens (state.current - 1)
  let peek state = List.nth state.tokens state.current
  let is_at_end state = (peek state).token_type = Eof
  let advance state : Token.token * state = (peek state, if is_at_end state then state else advance_current state)
  let check token_type state = if is_at_end state then false else (peek state).token_type = token_type

  let rec match_types token_types state =
    match token_types with
    | token :: rest -> if check token state then (true, advance_current state) else match_types rest state
    | _ -> (false, state)

  let consume token_type message state =
    if check token_type state then Ok (advance_current state) else Error (Unexpected_token (peek state, message))

  let synchronize state =
    let is_statement_boundary token_type =
      match token_type with
      | Token.Class
      | Fun
      | Var
      | For
      | If
      | While
      | Print
      | Return ->
          true
      | _ -> false
    in
    let rec consume_until_sync state =
      if is_at_end state || (previous state).token_type = Semicolon || is_statement_boundary (peek state).token_type
      then state
      else consume_until_sync (advance_current state)
    in
    state |> advance_current |> consume_until_sync

  let rec primary state =
    let current = peek state in
    match current.token_type with
    | Nil -> Ok (Ast.Literal None, advance_current state)
    | Bool -> (
        match current.literal with
        | Some (LBool bool) -> Ok (Ast.Literal (Some (LBool bool)), advance_current state)
        | _ -> Error (Unexpected_token (current, "Expected bool literal")))
    | Number -> (
        match current.literal with
        | Some (LFloat num) -> Ok (Ast.Literal (Some (LFloat num)), advance_current state)
        | _ -> Error (Unexpected_token (current, "Expected number literal")))
    | String -> (
        match current.literal with
        | Some (LString str) -> Ok (Ast.Literal (Some (LString str)), advance_current state)
        | _ -> Error (Unexpected_token (current, "Expected string literal")))
    | Left_paren ->
        let state = advance_current state in
        Result.bind (expression state) (fun (expr, state) ->
            Result.map (fun state -> (Ast.Grouping expr, state)) (consume Right_paren "Closing brace expected" state))
    | _ -> Error (Unexpected_token (current, "Expression expected"))

  and unary state =
    match match_types [ Bang; Minus ] state with
    | true, state ->
        let operator = previous state in
        Result.map (fun (rightExp, state) -> (Ast.Unary (operator, rightExp), state)) (unary state)
    | false, state -> primary state

  and factor state =
    let rec match_unary left state =
      match match_types [ Slash; Star ] state with
      | true, state ->
          let operator = previous state in
          Result.bind (unary state) (fun (right, state) ->
              let newExpression = Ast.Binary (left, operator, right) in
              match_unary newExpression state)
      | false, state -> Ok (left, state)
    in
    Result.bind (unary state) (fun (leftExp, state) -> match_unary leftExp state)

  and term state =
    let rec match_factors left state =
      match match_types [ Plus; Minus ] state with
      | true, state ->
          let operator = previous state in
          Result.bind (factor state) (fun (right, state) ->
              let newExpression = Ast.Binary (left, operator, right) in
              match_factors newExpression state)
      | false, state -> Ok (left, state)
    in
    Result.bind (factor state) (fun (leftExp, state) -> match_factors leftExp state)

  and comparison state =
    let rec match_terms left state =
      match match_types [ Greater; Greater_equal; Less; Less_equal ] state with
      | true, state ->
          let operator = previous state in
          let* right, state = term state in
          let newExpression = Ast.Binary (left, operator, right) in
          match_terms newExpression state
      | false, state -> Ok (left, state)
    in
    let* leftExp, state = term state in
    match_terms leftExp state

  and equality state =
    let rec match_comparisons left state =
      match match_types [ Bang_equal; Equal_equal ] state with
      | true, state ->
          let operator = previous state in
          Result.bind (comparison state) (fun (right, state) ->
              let newExpression = Ast.Binary (left, operator, right) in
              match_comparisons newExpression state)
      | false, state -> Ok (left, state)
    in
    Result.bind (comparison state) (fun (leftExp, state) -> match_comparisons leftExp state)

  and expression state = equality state

  let parse tokens = make_state tokens |> expression >>| fun (expr, _) -> expr
end

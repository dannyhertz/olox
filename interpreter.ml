[@@@warning "-39"]

open Ast
open Token
open Utils

module Interpreter = struct
  type value = LoxBool of bool | LoxNumber of float | LoxString of string | LoxNil

  type interpret_error =
    | Unhandled_expression of Ast.expression
    | Invalid_unary_operator of Token.token
    | Invalid_binary_operator of Token.token
    | Divide_by_zero

  let format_interpret_error = function
    | Unhandled_expression expr -> Printf.sprintf "Unhandled expression encountered: %s" (Ast.to_string expr)
    | Invalid_unary_operator token -> Printf.sprintf "Invalid unary operator '%s' at line %d." token.lexeme token.line
    | Invalid_binary_operator token -> Printf.sprintf "Invalid binary operator '%s' at line %d." token.lexeme token.line
    | Divide_by_zero -> "Divide by zero is illegal"

  let rec pretty_print_value = function
    | LoxBool b -> string_of_bool b
    | LoxNumber n -> string_of_float n
    | LoxString s -> "\"" ^ s ^ "\""
    | LoxNil -> "nil"

  let is_truthy = function
    | LoxBool b -> b
    | LoxNil -> false
    | _ -> true

  let rec equal_values v1 v2 =
    match (v1, v2) with
    | LoxBool b1, LoxBool b2 -> b1 = b2
    | LoxNumber n1, LoxNumber n2 -> n1 = n2
    | LoxString s1, LoxString s2 -> s1 = s2
    | LoxNil, LoxNil -> true
    | _ -> false

  let rec evaluate_expr expr =
    match expr with
    | Ast.Literal (Some (LFloat nVal)) -> Ok (LoxNumber nVal)
    | Ast.Literal (Some (LBool bVal)) -> Ok (LoxBool bVal)
    | Ast.Literal (Some (LString sVal)) -> Ok (LoxString sVal)
    | Ast.Literal None -> Ok LoxNil
    | Ast.Unary (token, expression) -> evaluate_unary token expression
    | Ast.Binary (l_expression, token, r_expression) -> evaluate_binary l_expression token r_expression
    | _ -> Error (Unhandled_expression expr)

  and evaluate_unary token expression =
    let* right = evaluate_expr expression in
    match (token.token_type, right) with
    | Token.Minus, LoxNumber num -> Ok (LoxNumber (-.num))
    | Token.Bang, v -> Ok (LoxBool (not (is_truthy v)))
    | _ -> Error (Invalid_unary_operator token)

  and evaluate_binary l_expression token r_expression =
    let* left = evaluate_expr l_expression in
    let* right = evaluate_expr r_expression in
    match (left, token.token_type, right) with
    | LoxNumber l_num, Token.Minus, LoxNumber r_num -> Ok (LoxNumber (l_num -. r_num))
    | LoxNumber _, Token.Slash, LoxNumber 0. -> Error Divide_by_zero
    | LoxNumber l_num, Token.Slash, LoxNumber r_num -> Ok (LoxNumber (l_num /. r_num))
    | LoxNumber l_num, Token.Star, LoxNumber r_num -> Ok (LoxNumber (l_num *. r_num))
    | LoxNumber l_num, Token.Plus, LoxNumber r_num -> Ok (LoxNumber (l_num +. r_num))
    | LoxString l_str, Token.Plus, LoxString r_str -> Ok (LoxString (l_str ^ r_str))
    | LoxNumber l_num, Token.Greater, LoxNumber r_num -> Ok (LoxBool (l_num > r_num))
    | LoxNumber l_num, Token.Greater_equal, LoxNumber r_num -> Ok (LoxBool (l_num >= r_num))
    | LoxNumber l_num, Token.Less, LoxNumber r_num -> Ok (LoxBool (l_num < r_num))
    | LoxNumber l_num, Token.Less_equal, LoxNumber r_num -> Ok (LoxBool (l_num <= r_num))
    | left, Token.Equal_equal, right -> Ok (LoxBool (equal_values left right))
    | left, Token.Bang_equal, right -> Ok (LoxBool (not (equal_values left right)))
    | _ -> Error (Invalid_binary_operator token)
end

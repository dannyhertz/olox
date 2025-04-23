open Token

module Ast = struct
  type expression =
    | Literal of Token.literal option
    | Binary of expression * Token.token * expression
    | Unary of Token.token * expression
    | Grouping of expression

  let rec to_string (expr : expression) : string =
    match expr with
    | Literal None -> "Nil"
    | Literal (Some l) -> (
      match l with
      | LFloat value -> string_of_float value
      | LString value -> value
      | LBool value -> string_of_bool value)
    | Unary (operator, ex) -> operator.lexeme ^ to_string ex
    | Binary (left, operator, right) -> to_string left ^ operator.lexeme ^ to_string right
    | Grouping expr -> "(" ^ to_string expr ^ ")"
end

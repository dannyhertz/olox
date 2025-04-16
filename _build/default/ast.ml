open Token

module Ast = struct
  type expression =
    | Literal of Token.literal option
    | Binary of expression * Token.token * expression
    | Unary of Token.token * expression
    | Grouping of expression

  let rec pretty_print (expr : expression) : string =
    match expr with
    | Literal None -> "Nil"
    | Literal (Some l) -> (
        match l with
        | LFloat value -> string_of_float value
        | LString value -> value
        | LBool value -> string_of_bool value)
    | Unary (operator, ex) -> operator.lexeme ^ pretty_print ex
    | Binary (left, operator, right) -> pretty_print left ^ operator.lexeme ^ pretty_print right
    | Grouping expr -> "(" ^ pretty_print expr ^ ")"
end

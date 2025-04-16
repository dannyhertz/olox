[@@@warning "-39"]

open Ast
open Token

module Interpreter = struct
  type value =
    | LoxBool of bool
    | LoxNumber of float
    | LoxString of string
    | LoxNil

  type interpret_error = Unexpected_token of Token.token * string

  let rec evaluate_expr expr =
    match expr with
    | Ast.Literal (Some (LFloat nVal)) -> LoxNumber nVal
    | Ast.Literal (Some (LBool bVal)) -> LoxBool bVal
    | Ast.Literal (Some (LString sVal)) -> LoxString sVal
    | Ast.Literal None -> LoxNil
    | _ -> failwith ""
end

module Token = struct
  type literal =
    | LFloat of float
    | LString of string
    | LBool of bool

  type token_type =
    (* Single-character tokens *)
    | Left_paren
    | Right_paren
    | Left_brace
    | Right_brace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    (* Single or Double-character tokens *)
    | Bang
    | Bang_equal
    | Equal
    | Equal_equal
    | Greater
    | Greater_equal
    | Less
    | Less_equal
    (* Literals *)
    | Identifier
    | String
    | Number
    | Bool
    (* Keywords *)
    | And
    | Class
    | Else
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | Var
    | While
    (* Special characters *)
    | Eof

  type token = {
    lexeme : string;
    line : int;
    token_type : token_type;
    literal : literal option;
  }

  let token_type_to_string = function
    | Left_paren -> "Left_paren"
    | Right_paren -> "Right_paren"
    | Left_brace -> "Left_brace"
    | Right_brace -> "Right_brace"
    | Comma -> "Comma"
    | Dot -> "Dot"
    | Minus -> "Minus"
    | Plus -> "Plus"
    | Semicolon -> "Semicolon"
    | Slash -> "Slash"
    | Star -> "Star"
    | Bang -> "Bang"
    | Bang_equal -> "Bang_equal"
    | Equal -> "Equal"
    | Equal_equal -> "Equal_equal"
    | Greater -> "Greater"
    | Greater_equal -> "Greater_equal"
    | Less -> "Less"
    | Less_equal -> "Less_equal"
    | Identifier -> "Identifier"
    | String -> "String"
    | Number -> "Number"
    | And -> "And"
    | Class -> "Class"
    | Else -> "Else"
    | Fun -> "Fun"
    | Bool -> "Bool"
    | For -> "For"
    | If -> "If"
    | Nil -> "Nil"
    | Or -> "Or"
    | Print -> "Print"
    | Return -> "Return"
    | Super -> "Super"
    | This -> "This"
    | Var -> "Var"
    | While -> "While"
    | Eof -> "Eof"

  let to_string token =
    Printf.sprintf "{ lexeme = %s; line = %d; token_type = %s; literal = %s }" token.lexeme token.line
      (token_type_to_string token.token_type)
      (match token.literal with
      | Some (LString s) -> Printf.sprintf "String_literal(%s)" s
      | Some (LFloat n) -> Printf.sprintf "Number_literal(%f)" n
      | Some (LBool b) -> Printf.sprintf "Bool_literal(%b)" b
      | None -> "None")
end

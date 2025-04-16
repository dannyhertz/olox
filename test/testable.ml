module Testable = struct
  open Token

  let token =
    let pp fmt (token : Token.token) =
      Format.fprintf fmt "{ lexeme = %s; line = %d; token_type = %s; literal = %s }" token.lexeme token.line
        (Token.token_type_to_string token.token_type)
        (match token.literal with
        | Some (LString s) -> Printf.sprintf "String(%s)" s
        | Some (LFloat n) -> Printf.sprintf "Float(%f)" n
        | Some (LBool b) -> Printf.sprintf "Bool(%b)" b
        | None -> "None")
    in
    let equal (t1 : Token.token) (t2 : Token.token) =
      t1.lexeme = t2.lexeme && t1.line = t2.line && t1.token_type = t2.token_type && t1.literal = t2.literal
    in
    Alcotest.testable pp equal
end

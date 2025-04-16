open Scanner
open Token
open Testable

let test_scan_single_character () =
  let source = "(" in
  match Scanner.scan_tokens source with
  | Ok tokens ->
      let expected =
        [
          { Token.lexeme = "("; line = 1; token_type = Token.Left_paren; literal = None };
          { lexeme = ""; line = 1; token_type = Token.Eof; literal = None };
        ]
      in
      Alcotest.(check (list Testable.token)) "single character scan" expected tokens
  | Error err -> Alcotest.fail (Scanner.scan_error_to_string err)

let test_scan_multiple_character () =
  let source = "(){}" in
  match Scanner.scan_tokens source with
  | Ok tokens ->
      let expected =
        [
          { Token.lexeme = "("; line = 1; token_type = Token.Left_paren; literal = None };
          { Token.lexeme = ")"; line = 1; token_type = Token.Right_paren; literal = None };
          { Token.lexeme = "{"; line = 1; token_type = Token.Left_brace; literal = None };
          { Token.lexeme = "}"; line = 1; token_type = Token.Right_brace; literal = None };
          { lexeme = ""; line = 1; token_type = Token.Eof; literal = None };
        ]
      in
      Alcotest.(check (list Testable.token)) "multiple character scan" expected tokens
  | Error err -> Alcotest.fail (Scanner.scan_error_to_string err)

let test_scan_conditional_characters () =
  let source = "!=" in
  match Scanner.scan_tokens source with
  | Ok tokens ->
      let expected =
        [
          { Token.lexeme = "!="; line = 1; token_type = Token.Bang_equal; literal = None };
          { lexeme = ""; line = 1; token_type = Token.Eof; literal = None };
        ]
      in
      Alcotest.(check (list Testable.token)) "conditional character scan" expected tokens
  | Error err -> Alcotest.fail (Scanner.scan_error_to_string err)

let test_scan_comments () =
  let source = "// This is a comment" in
  match Scanner.scan_tokens source with
  | Ok tokens ->
      let expected = [ { Token.lexeme = ""; line = 1; token_type = Token.Eof; literal = None } ] in
      Alcotest.(check (list Testable.token)) "comment scan" expected tokens
  | Error err -> Alcotest.fail (Scanner.scan_error_to_string err)

let test_scan_string_literal () =
  let source = "\"hello\"" in
  match Scanner.scan_tokens source with
  | Ok tokens ->
      let expected =
        [
          { Token.lexeme = "\"hello\""; line = 1; token_type = Token.String; literal = Some (Token.LString "hello") };
          { lexeme = ""; line = 1; token_type = Token.Eof; literal = None };
        ]
      in
      Alcotest.(check (list Testable.token)) "string literal scan" expected tokens
  | Error err -> Alcotest.fail (Scanner.scan_error_to_string err)

let test_scan_number_literal () =
  let source = "123.45" in
  match Scanner.scan_tokens source with
  | Ok tokens ->
      let expected =
        [
          { Token.lexeme = "123.45"; line = 1; token_type = Token.Number; literal = Some (Token.LFloat 123.45) };
          { lexeme = ""; line = 1; token_type = Token.Eof; literal = None };
        ]
      in
      Alcotest.(check (list Testable.token)) "number literal scan" expected tokens
  | Error err -> Alcotest.fail (Scanner.scan_error_to_string err)

let () =
  let open Alcotest in
  run "Scanner Tests"
    [
      ( "scan_tokens",
        [
          test_case "single character scan" `Quick test_scan_single_character;
          test_case "multiple character scan" `Quick test_scan_multiple_character;
          test_case "conditional character scan" `Quick test_scan_conditional_characters;
          test_case "comment scan" `Quick test_scan_comments;
          test_case "string literal scan" `Quick test_scan_string_literal;
          test_case "number literal scan" `Quick test_scan_number_literal;
        ] );
    ]

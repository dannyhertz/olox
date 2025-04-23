open Scanner
open Parser
open Utils
open Interpreter

type run_error =
  | ScanError of Scanner.scan_error
  | ParseError of Parser.parse_error
  | Interpreter of Interpreter.interpret_error

let run source =
  let open Result in
  match
    let* tokens = Scanner.scan_tokens source |> map_error (fun e -> ScanError e) in
    let* expr = Parser.parse tokens |> map_error (fun e -> ParseError e) in
    Interpreter.evaluate_expr expr |> map_error (fun e -> Interpreter e)
  with
  | Ok value -> print_endline (Interpreter.pretty_print_value value)
  | Error (ScanError e) -> print_endline (Scanner.scan_error_to_string e)
  | Error (ParseError e) -> print_endline (Parser.format_parse_error e)
  | Error (Interpreter e) -> print_endline (Interpreter.format_interpret_error e)

let run_prompt () =
  while true do
    print_string "> " ;
    run (read_line ())
  done

let run_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len |> run)

(* this should return a result *)
let main args =
  match Array.length args with
  | 1 -> run_prompt ()
  | 2 -> run_file args.(1)
  | _ ->
    print_endline "Usage: olox [script]" ;
    exit 64

(** Entry point for Olox. *)
let () = main Sys.argv

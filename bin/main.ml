open Scanner
open Parser
open Ast

let run source =
  match Scanner.scan_tokens source with
  | Error err -> Scanner.scan_error_to_string err |> print_string
  | Ok tokens -> (
      match Parser.parse tokens with
      | Ok expr -> print_endline (Ast.pretty_print expr)
      | Error err -> print_endline (Parser.format_parse_error err))

let run_prompt () =
  while true do
    print_string "> " ;
    run (read_line ()) |> ignore
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

open Core
open Lexer
open Lexing

let print_err_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d, Column:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  try Ok (Parser.program Lexer.read_token lexbuf) with
  | SyntaxError msg ->
      let err_msg = Fmt.str "%s: %s@." (print_err_position lexbuf) msg in
      Error (Error.of_string err_msg)
  | Parser.Error    ->
      let error_msg = Fmt.str "%s: Syntax error@." (print_err_position lexbuf) in
      Error (Error.of_string error_msg)

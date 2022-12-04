(** Executes the lexer and parser. Act as an interface between the parsing code and the
    main code. *)
open Core

val parse_program : Lexing.lexbuf -> Parsed_ast.program Or_error.t

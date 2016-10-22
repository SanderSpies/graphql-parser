open Graphql_lexer
open Lexing
open Format

let chan_input = ref "";;

let parse_with_error lexbuf =
try Graphql_parser.prog Graphql_lexer.token lexbuf with
  | Error (err, loc) -> Graphql_lexer.report_error std_formatter err; ""
;;

let rec parse_and_print lexbuf =
  parse_with_error lexbuf
;;

let () =
  let file_chan = open_in "test.graphql" in
    seek_in file_chan 0;
  let lexbuf = Lexing.from_channel file_chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "test.graphql" };
  print_string (parse_and_print lexbuf);

;;

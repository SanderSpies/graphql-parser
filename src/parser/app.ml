open Graphql_lexer
open Lexing
open Format
open Syntax_error

let chan_input = ref "";;

let format_location loc =
  (" at " ^ string_of_int(Location.(loc.loc_start.pos_lnum)) ^ ":" ^ string_of_int(Location.(loc.loc_start.pos_cnum - loc.loc_start.pos_bol) + 1) ^  " - ") ^
  (string_of_int(Location.(loc.loc_end.pos_lnum)) ^ ":" ^ string_of_int(Location.(loc.loc_end.pos_cnum - loc.loc_end.pos_bol) + 1)) ^ "\n"


let parse_with_error lexbuf =
try Graphql_parser.prog Graphql_lexer.token lexbuf with
  | Error (err, loc) -> (Graphql_lexer.report_error std_formatter err; (format_location loc))
  | Graphql_parser.Error -> let pos = lexbuf.lex_curr_p in "Syntax error\n" ^
      (pos.pos_fname ^ " at " ^ string_of_int(pos.pos_lnum) ^ ":" ^ string_of_int(pos.pos_cnum - pos.pos_bol + 1)) ^ "\n"
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

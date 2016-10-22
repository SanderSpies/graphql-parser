{
open Lexing
open Graphql_parser

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Literal_overflow of string

exception Error of error * Location.t

let remove_underscores s =
  let l = String.length s in
  let b = Bytes.create l in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else Bytes.sub_string b 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> Bytes.set b dst c; remove (src + 1) (dst + 1)
  in remove 0 0

let cvt_int_literal s =
  - int_of_string ("-" ^ s)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
    | None -> pos.pos_fname
    | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0
let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= Bytes.length !string_buff then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
    Bytes.blit !string_buff 0 new_buff 0 (Bytes.length !string_buff);
    string_buff := new_buff
  end;
  Bytes.unsafe_set !string_buff !string_index c;
  incr string_index

let store_string s =
  for i = 0 to String.length s - 1 do
    store_string_char s.[i];
  done

let store_lexeme lexbuf =
  store_string (Lexing.lexeme lexbuf)

let get_stored_string () =
  let s = Bytes.sub_string !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s

let string_start_loc = ref Location.none
let comment_start_loc = ref []
let in_comment () = !comment_start_loc <> []
let is_in_string = ref false
let in_string () = !is_in_string
(*let print_warnings = ref true*)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                    Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable 149 [
    "enum", ENUM;
    "extend", EXTEND;
    "false", FALSE;
    "implements", IMPLEMENTS;
    "input", INPUT;
    "interface", INTERFACE;
    "scalar", SCALAR;
    "schema", SCHEMA;
    "true", TRUE;
    "type", TYPE;
    "union", UNION;
    "directive", DIRECTIVE;
    "on", ON;
    "QUERY", QUERY_CAPS;
    "MUTATION", MUTATION_CAPS;
    "FIELD", FIELD_CAPS;
    "FRAGMENT_DEFINITION", FRAGMENT_DEFINITION_CAPS;
    "FRAGMENT_SPREAD", FRAGMENT_SPREAD_CAPS;
    "INLINE_FRAGMENT", INLINE_FRAGMENT_CAPS;
  ]

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
      fprintf ppf "This comment contains an unterminated string literal@.\
                   %aString literal begins here"
              Location.print_error loc
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Literal_overflow ty ->
      fprintf ppf "Integer literal exceeds the range of representable \
                   integers of type %s" ty

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
          None
    )
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\020']

(* 2.19 *)
let name_chars =  ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_'  '0'-'9']*
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token = parse
| blank { token lexbuf }
| newline
    {
      update_loc lexbuf None 1 false 0;
      token lexbuf
    }
| "#"
    {
      let loc = Location.curr lexbuf  in
      comment_start_loc := [loc];
      reset_string_buffer ();
      let _ = comment lexbuf in
      let _ = get_stored_string () in
      reset_string_buffer ();
      token lexbuf
    }
| name_chars
    {
      let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT s
    }
| int_literal
    {
      try
        INT (cvt_int_literal (Lexing.lexeme lexbuf))
      with Failure _ ->
        raise (Error(Literal_overflow "int32", Location.curr lexbuf))
    }
| float_literal
    { FLOAT (remove_underscores(Lexing.lexeme lexbuf)) }
| "\""
    { reset_string_buffer();
      is_in_string := true;
      let string_start = lexbuf.lex_start_p in
      string_start_loc := Location.curr lexbuf;
      string lexbuf;
      is_in_string := false;
      lexbuf.lex_start_p <- string_start;
      STRING (get_stored_string()) }
| "type" { TYPE }
| "{" { LBRACE }
| "}" { RBRACE }
| ":" { COLON }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "enum" { ENUM }
| "interface" { INTERFACE }
| "scalar" { SCALAR }
| "schema" { SCHEMA }
| "@" { AT }
| "union" { UNION }
| "|" { BAR }
| "=" { EQUAL }
| "extend" { EXTEND }
| "implements" { IMPLEMENTS }
| "(" { LPAREN }
| ")" { RPAREN }
| "," { COMMA }
| "!" { BANG }
| "input" { INPUT }
| "directive" { DIRECTIVE }
| "on" { ON }
| "QUERY" { QUERY_CAPS }
| "MUTATION" { MUTATION_CAPS }
| "FIELD" { FIELD_CAPS }
| "FRAGMENT_DEFINITION" { FRAGMENT_DEFINITION_CAPS }
| "FRAGMENT_SPREAD" { FRAGMENT_SPREAD_CAPS }
| "INLINE_FRAGMENT" { INLINE_FRAGMENT_CAPS }
| eof { EOF }
| _
    {
      raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0), Location.curr lexbuf))
    }

and comment = parse
  | newline | eof
      {
        match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                  store_lexeme lexbuf;
                  comment lexbuf;
      }
  | _
      { store_lexeme lexbuf; comment lexbuf }

and string = parse
    '"'
      { () }
  | newline
      {
        if not (in_comment ()) then
        Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      }
  | eof
      {
        is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc))
      }
  | _
      {
        store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf
      }

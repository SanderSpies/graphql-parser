type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Literal_overflow of string
  | BadThing of string

exception Error of error * Location.t

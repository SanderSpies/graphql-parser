%{
  open Location
  open Syntax_error

  let ast = ref Ast.{
    schema = None;
    scalars = [];
    typeDeclarations = [];
    interfaces = [];
    unions = [];
    enums = [];
    extends = [];
    inputs = [];
    directives = [];
  }

  let mklocation loc_start loc_end = {
    loc_start = loc_start;
    loc_end = loc_end;
    loc_ghost = false;
  }

%}

%token AT
%token BANG
%token BAR
%token COLON
%token COMMA
%token DIRECTIVE
%token ENUM
%token EOF
%token EQUAL
%token EXTEND
%token FALSE
%token FIELD_CAPS
%token <string> FLOAT
%token FRAGMENT_DEFINITION_CAPS
%token FRAGMENT_SPREAD_CAPS
%token <string> IDENT
%token IMPLEMENTS
%token INLINE_FRAGMENT_CAPS
%token INPUT
%token <int> INT
%token INTERFACE
%token LBRACE
%token LBRACKET
%token LPAREN
%token MUTATION
%token MUTATION_CAPS
%token ON
%token QUERY
%token QUERY_CAPS
%token RBRACE
%token RBRACKET
%token RPAREN
%token SCALAR
%token SCHEMA
%token <string> STRING
%token TRUE
%token TYPE
%token UNION

%start <string> prog

%%

prog:
| schema prog
  {
    ast := Ast.{!ast with schema = $1};
    $2
  }
| scalar prog
  {
    ast := Ast.{!ast with scalars = !ast.scalars @ [$1]};
    $2
  }
| type_declaration prog
  {
    ast := Ast.{!ast with typeDeclarations = !ast.typeDeclarations};
    $2
  }
| interface prog { $1 ^ $2 }
| union prog { $1 ^ $2 }
| enum prog { $1 ^ $2 }
| extend prog { $1 ^ $2 }
| input prog { $1 ^ $2 }
| directive prog { $1 ^ $2 }
| EOF { "" }
;

/* extend type ... */
extend:
| EXTEND type_declaration {
    "extend " ^ $2
  }
;

/* union foo = A | B | C */
union:
| UNION ident opt_annotation EQUAL union_list { "union " ^ $2 ^ $3 ^ " = " ^ $5 ^ "\n" }
;
union_item:
| ident { $1 }
;

union_list:
| union_item more_unions { $1 ^ " " ^ $2 }
;

more_unions:
| /* */ { "" }
| BAR union_item more_unions { $2 ^ " " ^ $3 }
;

/* @annotation*/
opt_annotation:
| /* */ { "" }
| AT ident opt_annotation_arguments opt_annotation { "@" ^ $2 ^ $3 ^ $4 }
;
opt_annotation_arguments:
| /* */ { "" }
| LPAREN annotation_arguments RPAREN { "(" ^ $2 ^ ")" }
;
annotation_arguments:
| ident opt_annotation_arg_value more_annotation_arguments { $1 ^ $2 ^ $3}
;
opt_annotation_arg_value:
| COLON constant { ":" ^ $2 }
;
more_annotation_arguments:
| /* */ { "" }
| COMMA annotation_arguments { "," ^ $2 }
;

/* schema { query: start, mutation: start, else: start} */
schema:
  | SCHEMA LBRACE RBRACE
    {
      let loc = mklocation $symbolstartpos $endpos in
      raise (Syntax_error.Error(BadThing "A schema is expected to have at least a query field", loc))
    }
  | SCHEMA LBRACE schema_properties RBRACE
    {
      let (query, mutation) = $3 in
      if query = None then
        let loc = mklocation $symbolstartpos $endpos in
        raise (Syntax_error.Error(BadThing "A schema is expected to have at least a query field", loc))
      else
      Some Ast.{
        query = query;
        mutation = mutation;
      }
    }
;

schema_properties:
  | QUERY COLON ident opt_mutation {
    (Some $3, $4)
  }
  | MUTATION COLON ident opt_query {
    ($4, Some $3)
  }
;

opt_mutation:
  | /* */ { None }
  | MUTATION COLON IDENT { Some $3 }
;

opt_query:
  | /* */ { None }
  | QUERY COLON IDENT { Some $3 }
;

opt_implements:
| /* */ { "" }
| IMPLEMENTS ident { "implements " ^ $2}
;

properties:
| /* */ { "" }
| property opt_annotation properties { $1 ^ $2 ^ "\n" ^ $3}
;

/* type foo */
type_declaration:
| TYPE ident opt_annotation opt_implements LBRACE properties_with_optional_default RBRACE { "type " ^ $2 ^ $3 ^ $4 ^ " {\n" ^ $6 ^ "}\n"  }
;

properties_with_optional_default:
| /* */ { "" }
| property opt_default_value opt_annotation properties_with_optional_default { $1 ^ $2 ^ "\n" ^ $3 ^ $4}
;

property:
| ident opt_arguments COLON graphtype { "  " ^ $1 ^ $2 ^ " : " ^ $4 }
;

opt_arguments:
| /* */ { "" }
| LPAREN arguments RPAREN { "(" ^ $2 ^ ")" }
;

arguments:
| property opt_default_value opt_annotation more_arguments { $1 ^ $2 ^ $3 ^ $4}
;

more_arguments:
| /* */ { "" }
| COMMA arguments { ", " ^ $2 }
;

opt_default_value:
| /* */ { "" }
| EQUAL constant { "=" ^ $2 }
;

strings_list:
  | STRING more_strings { "\"" ^ $1 ^ "\"" ^ $2}
;

more_strings:
  | /* */ { "" }
  | COMMA strings_list { "," ^ $2}
;


constant:
| LBRACKET strings_list RBRACKET  { "[" ^ $2 ^ "]" }
| STRING { "\"" ^ $1 ^ "\"" }
| INT { (string_of_int $1) }
| FLOAT { $1 }
| TRUE { "true" }
| FALSE { "false" }
;

graphtype:
| ident opt_notnull { $1 ^ $2 }
| LBRACKET ident RBRACKET { "list " ^ $2}
;

opt_notnull:
/* */ { "" }
| BANG { "!" }
;

enum:
| ENUM ident opt_annotation LBRACE ident_list RBRACE { "enum " ^ $2 ^ $3 ^ " {\n" ^ $5 ^ "}\n" }
;

ident_list:
| /* empty */ { "" }
| ident opt_annotation ident_list { "  " ^ $1 ^ $2 ^ "\n" ^ $3 }
;

scalar:
| SCALAR ident opt_annotation
    {
      Ast.{
        title = $2;
        annotations = []; (* TODO *)
      }
    }
;

interface:
| INTERFACE ident opt_annotation LBRACE properties RBRACE { "interface " ^ $2 ^ $3 ^ " {\n" ^ $5 ^ "}\n"  }
;

input:
| INPUT ident opt_annotation LBRACE properties_with_optional_default RBRACE { "input " ^ $2 ^ $3 ^ "{\n" ^ $5 ^ "}\n"}
;

directive:
| DIRECTIVE AT ident LPAREN arguments RPAREN ON directive_location { "directive @" ^ $3 ^ "(" ^ $5 ^ ") on " ^ $8 ^ "\n" }

directive_location:
| QUERY_CAPS more_directive_location { "QUERY" ^ $2 }
| MUTATION_CAPS more_directive_location { "MUTATION" ^ $2 }
| FIELD_CAPS more_directive_location { "FIELD" ^ $2 }
| FRAGMENT_DEFINITION_CAPS more_directive_location { "FRAGMENT_DEFINITION" ^ $2 }
| FRAGMENT_SPREAD_CAPS more_directive_location { "FRAGMENT_SPREAD" ^ $2 }
| INLINE_FRAGMENT_CAPS more_directive_location { "INLINE_FRAGMENT" ^ $2 }
;

more_directive_location:
| /* */ { "" }
|  BAR directive_location { " | " ^ $2 }
;

ident:
| IDENT { $1 }
| TYPE { "type" }
| ENUM { "enum" }
| FALSE { "false" }
| IMPLEMENTS { "implements" }
| INPUT { "input" }
| INTERFACE { "interface" }
| SCALAR { "scalar" }
| SCHEMA { "schema" }
| TRUE { "true" }
| UNION { "union" }
| DIRECTIVE { "directive" }
| ON { "on" }
| QUERY_CAPS { "QUERY" }
| MUTATION_CAPS { "MUTATION" }
| FIELD_CAPS { "FIELD" }
| FRAGMENT_DEFINITION_CAPS { "FRAGMENT_DEFINITION" }
| FRAGMENT_SPREAD_CAPS { "FRAGMENT_SPREAD" }
| INLINE_FRAGMENT_CAPS { "INLINE_FRAGMENT" }
;

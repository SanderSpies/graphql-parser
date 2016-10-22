rm graphql_parser.conflicts
ocamlbuild -use-menhir -tag thread -package compiler-libs.common -quiet app.native
cp _build/*.conflicts .
./app.native

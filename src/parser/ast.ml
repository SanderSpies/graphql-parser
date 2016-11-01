type property = {
  name: string;
  value: string;
}

type typeDeclaration = {
  directives: int;
  properties: property list;
  title: string;
}

type interface = {
  directives: int;
  properties: property list;
  title: string;
}

type union = {
  title: string
}

type schema = {
  query: string option;
  mutation: string option;
}

type scalar = {
  title: string;
  annotations: string list
}

type enum = {
  title: string;
}

type extend = {
  title: string;
}

type input = {
  title: string;
}

type directive = {
  title: string;
}

type unstructured_ast = {
  schema: schema option;
  scalars: scalar list;
  typeDeclarations: typeDeclaration list;
  interfaces: interface list;
  unions: union list;
  enums: enum list;
  extends: extend list;
  inputs: input list;
  directives: directive list;
}

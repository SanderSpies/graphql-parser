type property = {
  name: string;
  value: string;
}

type typeDefinition = {
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

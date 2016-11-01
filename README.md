End-user experience
---
- npm i reason-graphql-server
- Write a schema
- Write a resolver
- npm start

*boom it works*


0.1 - Generate simple GraphQL unikernel
===
1. Parse `*.graphql` to an unstructured AST
2. Validate and create a structured AST
3. Create a CoHTTP implementation
4. Generate query request handler with big pattern match

---

0.2 -
===



Goals:
- share types between server and client
- easily deploy a new version

Problem areas:
- debugging?
- No HTTP2 :(

```
- some schema dude
```

```
GraphQLServer.resolver (fun
  | A => {

  }
  | B => {

  }
)
```

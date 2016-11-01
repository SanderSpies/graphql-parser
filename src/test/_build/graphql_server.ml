open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    let url = Uri.path (Request.uri req) in
    match url with
    | "/query" ->
      (print_string(Cohttp_lwt_body.to_string  body);
      Server.respond_string ~status:`OK ~body:"graphql endpoint yo" ())
    | _ -> Server.respond_string ~status:`OK ~body:"other" ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)


(* let server => Server.run port::3000 (fun request => {
  switch request.url {

  };
}); *)

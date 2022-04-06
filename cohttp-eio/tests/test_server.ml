open Cohttp_eio

let app req =
  let body = match Request.read_fixed req with Ok s -> s | Error _ -> "" in
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  Request.pp fmt req;
  Format.fprintf fmt "\n\n%s%!" body;
  Response.text (Buffer.contents buf)

let () =
  let server = Server.create ~port:8080 app ~socket_backlog:128 in
  Eio_main.run @@ fun env -> Server.run server (env :> Eio.Stdenv.t)

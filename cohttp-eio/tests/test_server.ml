open Cohttp_eio

let app req =
  match Request.resource req with
  | "/" ->
      let body = match Request.read_fixed req with Ok s -> s | Error _ -> "" in
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      Request.pp fmt req;
      Format.fprintf fmt "\n\n%s" body;
      Response.text (Buffer.contents buf)
  | _ -> Response.not_found

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";

  let server = Server.create ~port:!port app ~socket_backlog:128 in
  Eio_main.run @@ fun env -> Server.run server (env :> Eio.Stdenv.t)

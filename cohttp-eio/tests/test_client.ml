module Net = Eio.Net
module Stdenv = Eio.Stdenv
module Switch = Eio.Switch
open Cohttp_eio

let conn env sw _scheme (host, port) =
  let he = Unix.gethostbyname host in
  let addr = `Tcp (Eio_unix.Ipaddr.of_unix he.h_addr_list.(0), port) in
  (Net.connect ~sw (Stdenv.net env) addr :> Eio.Flow.two_way)

let url port path =
  Printf.sprintf "http://localhost:%d/%s" port path |> Uri.of_string

let get env sw port =
  let res =
    Client.get
      ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
      (conn env sw) (url port "get")
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let post env sw port =
  let content = "hello world!" in
  let content_length = String.length content |> string_of_int in
  let res =
    Client.post
      ~headers:
        (Http.Header.of_list
           [
             ("Accept", "application/json"); ("Content-Length", content_length);
           ])
      ~body:(Body.Fixed content) (conn env sw) (url port "post")
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let invalid_uri env sw =
  let res =
    Client.get
      ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
      (conn env sw) (Uri.of_string "/get")
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let () =
  let port = ref 8080 in
  let t = ref "invalid_uri" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Server listening port number(8080 default)");
      ( "-t",
        Arg.Set_string t,
        "Specify test case to execute,('invalid_uri' default)" );
    ]
    ignore "An HTTP/1.1 server";

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  match !t with
  | "get" -> get env sw !port
  | "post" -> post env sw !port
  | "invalid_uri" -> invalid_uri env sw
  | _ -> print_string "Usage: test-client [get|post|invalid_uri]"

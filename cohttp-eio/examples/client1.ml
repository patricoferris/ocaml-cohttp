open Eio
open Cohttp_eio

let conn env sw _scheme (host, port) =
  let he = Unix.gethostbyname host in
  let addr = `Tcp (Eio_unix.Ipaddr.of_unix he.h_addr_list.(0), port) in
  (Net.connect ~sw (Stdenv.net env) addr :> Eio.Flow.two_way)

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let uri = Uri.of_string "http://www.example.org/" in
  let res = Client.get (conn env sw) uri in
  match Client.read_fixed res with Some b -> print_string b | None -> ()

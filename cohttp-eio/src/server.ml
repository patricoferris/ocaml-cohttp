open Eio.Std

type handler = Request.t -> Response.t
type middleware = handler -> handler

type t = {
  socket_backlog : int;
  domains : int;
  port : int;
  request_handler : handler;
}

let domain_count =
  match Sys.getenv_opt "COHTTP_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

let read_fn flow buf ~off ~len =
  try
    let cs = Cstruct.of_bigarray ~off ~len buf in
    Eio.Flow.read flow cs
  with End_of_file -> 0

let rec handle_request t sw request writer flow =
  match Request.parse_into request with
  | () ->
      let response = t.request_handler request in
      Response.write response writer;
      Writer.wakeup writer;
      if Request.is_keep_alive request then (
        Request.clear request;
        handle_request t sw request writer flow)
      else Eio.Flow.close flow
  | exception End_of_file -> Eio.Flow.close flow
  | exception Parser.Parse_failure _ ->
      Response.(write bad_request writer);
      Writer.wakeup writer;
      Eio.Flow.close flow
  | exception _ ->
      Response.(write internal_server_error writer);
      Writer.wakeup writer;
      Eio.Flow.close flow

let run_domain (t : t) ssock =
  traceln "Running server in domain %d" (Domain.self () :> int);
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  Switch.run (fun sw ->
      while true do
        Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error
          (fun ~sw flow _addr ->
            let reader = Reader.create 1024 (flow :> Eio.Flow.source) in
            let request = Request.create reader in
            let writer = Writer.create flow in
            Eio.Fiber.fork ~sw (fun () -> Writer.run writer);
            handle_request t sw request writer flow)
      done)

let create ?(socket_backlog = 128) ?(domains = domain_count) ~port
    request_handler =
  { socket_backlog; domains; port; request_handler }

(* wrk2 -t 24 -c 1000 -d 60s -R400000 http://localhost:8080 *)
let run (t : t) (env : Eio.Stdenv.t) =
  Eio.Std.traceln "\nServer listening on 127.0.0.1:%d" t.port;
  Eio.Std.traceln "\nStarting %d domains ...%!" t.domains;
  Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let ssock =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~reuse_port:true
      ~backlog:t.socket_backlog
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, t.port))
  in
  for _ = 2 to t.domains do
    Eio.Std.Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_domain t ssock))
  done;
  run_domain t ssock

(* Basic handlers *)

let not_found _ = Response.not_found

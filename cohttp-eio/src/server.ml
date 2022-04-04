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

let rec handle_request t reader writer flow =
  match Request.parse reader with
  | request ->
    let response = t.request_handler request in
      Response.write response reader writer;
      Writer.wakeup writer;
      if Request.is_keep_alive request then (
        (if not request.read_complete then
        match Http.Header.get_transfer_encoding (Request.headers request) with
        | Http.Transfer.Fixed _ -> ignore @@ Request.read_fixed request
        | Http.Transfer.Chunked -> ignore @@ Request.read_chunk request ignore
        | _ -> ());
        handle_request t reader writer flow)
      else Eio.Flow.close flow
  | (exception End_of_file) | (exception Eio.Net.Connection_reset _) ->
      Eio.Flow.close flow
  | exception Parser.Parse_failure _e ->
      Response.(write bad_request reader writer);
      Writer.wakeup writer;
      Eio.Flow.close flow
  | exception _ ->
      Response.(write internal_server_error reader writer);
      Writer.wakeup writer;
      Eio.Flow.close flow

let run_domain (t : t) ssock =
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  Switch.run (fun sw ->
      while true do
        Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error
          (fun ~sw flow _addr ->
            let reader = Reader.create 0x1000 (flow :> Eio.Flow.source) in
            let writer = Writer.create flow in
            Eio.Fiber.fork ~sw (fun () -> Writer.run writer);
            handle_request t reader writer flow)
      done)

let create ?(socket_backlog = 128) ?(domains = domain_count) ~port
    request_handler =
  { socket_backlog; domains; port; request_handler }

(* wrk2 -t 24 -c 1000 -d 60s -R400000 http://localhost:8080 *)
let run (t : t) (env : Eio.Stdenv.t) =
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

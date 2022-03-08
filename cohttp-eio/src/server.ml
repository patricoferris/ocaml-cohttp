open Eio.Std

type handler = Request.t * Response.t -> unit
type middleware = handler -> handler

type t = {
  socket_backlog : int;
  domains : int;
  port : int;
  request_handler : handler;
  stopped : bool Atomic.t;
}

let stop t = ignore @@ Atomic.compare_and_set t.stopped false true

let domain_count =
  match Sys.getenv_opt "COHTTP_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

let read_fn flow buf ~off ~len =
  try
    let cs = Cstruct.of_bigarray ~off ~len buf in
    Eio.Flow.read flow cs
  with End_of_file -> 0

let handle_request (t : t) flow _addr : unit =
  let reader = Reader.create 1024 (read_fn flow) in
  let request = Request.create reader in
  let response =
    Response.(create (flow :> Eio.Flow.sink) (Header.create 15) Empty)
  in
  let rec loop () =
    match Request.parse_into request with
    | () -> (
        t.request_handler (request, response);
        Response.write response;
        if Atomic.get t.stopped then Eio.Flow.close flow
        else
          match (Request.is_keep_alive request, request.version) with
          | true, Version.HTTP_1_0 | _, Version.HTTP_1_1 ->
              Request.clear request;
              Response.clear response;
              loop ()
          | false, Version.HTTP_1_0 -> Eio.Flow.close flow)
    | exception End_of_file ->
        Eio.traceln "Connection closed by client";
        Eio.Flow.close flow
    | exception Parser.Parse_failure msg ->
        Printf.eprintf "\nRequest parsing error: %s%!" msg;
        Response.bad_request response;
        Response.write response;
        Eio.Flow.close flow
    | exception exn ->
        Printf.eprintf "\nUnhandled exception: %s%!" (Printexc.to_string exn);
        Response.internal_server_error response;
        Response.write response;
        Eio.Flow.close flow
  in
  loop ()

let run_domain (t : t) ssock =
  traceln "Running server in domain %d" (Domain.self () :> int);
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  Switch.run (fun sw ->
      while not (Atomic.get t.stopped) do
        Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error
          (fun ~sw:_ flow addr -> handle_request t flow addr)
      done)

let create ?(socket_backlog = 10_000) ?(domains = domain_count) ~port
    request_handler =
  {
    socket_backlog;
    domains;
    port;
    request_handler;
    stopped = Atomic.make false;
  }

(* wrk2 -t 24 -c 1000 -d 60s -R400000 http://localhost:8080 *)
let run (t : t) (env : Eio.Stdenv.t) =
  Eio.Std.traceln "\nServer listening on 127.0.0.1:%d" t.port;
  Eio.Std.traceln "\nStarting %d domains ...%!" t.domains;
  Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let ssock =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true
      ~backlog:t.socket_backlog
    @@ `Tcp (Eio.Net.Ipaddr.V4.loopback, t.port)
  in
  for _ = 2 to t.domains do
    Eio.Std.Fibre.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_domain t ssock))
  done;
  run_domain t ssock

(* Basic handlers *)

let not_found (_, response) = Response.not_found response

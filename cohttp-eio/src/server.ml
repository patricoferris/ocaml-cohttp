open Eio.Std

module Client_connection = struct
  type t = {
    flow : < Eio.Flow.two_way ; Eio.Flow.close >;
    switch : Eio.Std.Switch.t;
    addr : Eio.Net.Sockaddr.t;
    ic : Eio.Flow.read;
    oc : Eio.Flow.write;
  }

  let client_addr t = t.addr
  let switch t = t.switch
  let close t = Eio.Flow.close t.flow
  let ic t = t.ic
  let oc t = t.oc
end

type response =
  [ `Response of Http.Response.t * Cstruct.t
  | `Expert of Client_connection.t -> unit ]

type request_handler = Request.t * Request.body -> response

type t = {
  socket_backlog : int;
  domains : int;
  port : int;
  request_handler : request_handler;
  closed : bool Atomic.t;
}

let close t = ignore @@ Atomic.compare_and_set t.closed false true

let cpu_core_count =
  match Sys.os_type with
  | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")
  | _ -> (
      let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let close () = ignore (Unix.close_process_in i) in
      try
        let in_channel = Scanf.Scanning.from_channel i in
        Scanf.bscanf in_channel "%d" (fun n ->
            close ();
            n)
      with e ->
        close ();
        raise e)
  | (exception Not_found)
  | (exception Sys_error _)
  | (exception Failure _)
  | (exception Scanf.Scan_failure _)
  | (exception End_of_file)
  | (exception Unix.Unix_error (_, _, _)) ->
      1

let write_response (_client_conn : Client_connection.t) _response = ()

let handle_request (_req : Http.Request.t) =
  let res = Http.Response.make ~status:`OK () in
  let body = "Hello" in
  (res, body)

let handle_client (_t : t) (client_conn : Client_connection.t) : unit =
  let rec loop_requests unconsumed =
    let open Parser in
    match parse request client_conn.ic unconsumed with
    | unconsumed, req ->
        let res, _body = handle_request req in
        let keep_alive = Http.Request.is_keep_alive req in
        let response_headers =
          Http.Header.add_unless_exists
            (Http.Response.headers res)
            "connection"
            (if keep_alive then "keep-alive" else "close")
        in
        let res = { res with headers = response_headers } in
        write_response client_conn res;
        loop_requests unconsumed
    | exception Eof -> Client_connection.close client_conn
    | exception Parse_error msg ->
        Printf.eprintf "Request parsing error: %s" msg;
        Http.Response.make ~version:`HTTP_1_1 ~status:`Bad_request ()
        |> write_response client_conn
  in
  loop_requests Cstruct.empty

let run_accept_loop (t : t) sw env =
  let net = Eio.Stdenv.net env in
  let sockaddr = `Tcp (Unix.inet_addr_loopback, t.port) in
  let ssock =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog:t.socket_backlog
      ~sw net sockaddr
  in
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  while not (Atomic.get t.closed) do
    Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error (fun ~sw flow addr ->
        let client_conn =
          Client_connection.
            {
              flow;
              addr;
              switch = sw;
              ic = (flow :> Eio.Flow.read);
              oc = (flow :> Eio.Flow.write);
            }
        in
        handle_client t client_conn)
  done

let create ?(socket_backlog = 10_000) ?(domains = cpu_core_count) ~port
    request_handler : t =
  { socket_backlog; domains; port; request_handler; closed = Atomic.make false }

(* wrk2 -t10 -c400 -d30s -R2000 http://localhost:3000 *)
let run (t : t) =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  (* Run accept loop on domain0 without creating a Domain.t *)
  run_accept_loop t sw env;
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  for _ = 2 to t.domains do
    Fibre.fork ~sw @@ fun () ->
    Eio.Domain_manager.run domain_mgr @@ fun () -> run_accept_loop t sw env
  done

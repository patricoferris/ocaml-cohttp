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

type request = Request.t * Request.body option
type response = Http.Response.t * Cstruct.t option
type handler = request -> response
type middleware = handler -> handler

type t = {
  socket_backlog : int;
  domains : int;
  port : int;
  request_handler : handler;
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

let write_response (_client_conn : Client_connection.t) (_res, _body) = ()

let request_body (conn : Client_connection.t) req (unconsumed : Cstruct.t ref)
    (read_chunk_complete : bool ref) : Request.body option =
  match (Request.has_body req, Request.encoding req) with
  | `Yes, Http.Transfer.Chunked ->
      let total_read = ref 0 in
      let rec read_chunk f =
        if !read_chunk_complete then `Eof
        else
          let u, chunk =
            Parser.(parse (chunk !total_read req) conn.ic !unconsumed)
          in
          unconsumed := u;
          match chunk with
          | Request.Chunk x as c ->
              f c;
              total_read := !total_read + x.length;
              (read_chunk [@tailcall]) f
          | Request.Last_chunk _ as c ->
              read_chunk_complete := true;
              f c;
              `Ok
      in
      Some (`Chunked read_chunk)
  | `Yes, Http.Transfer.Fixed content_length ->
      let content_length = Int64.to_int content_length in
      let u, body =
        Parser.(parse (fixed_body content_length) conn.ic !unconsumed)
      in
      unconsumed := u;
      body
  | _, _ -> None

let rec handle_request (t : t) (conn : Client_connection.t)
    (unconsumed : Cstruct.t ref) : unit =
  let open Parser in
  match parse request conn.ic !unconsumed with
  | u, req ->
      unconsumed := u;
      let read_chunk_complete = ref false in
      let req_body = request_body conn req unconsumed read_chunk_complete in
      let res, res_body = t.request_handler (req, req_body) in
      let keep_alive = Http.Request.is_keep_alive req in
      let response_headers =
        Http.Header.add_unless_exists
          (Http.Response.headers res)
          "connection"
          (if keep_alive then "keep-alive" else "close")
      in
      let res = { res with headers = response_headers } in
      write_response conn (res, res_body);
      if not keep_alive then Client_connection.close conn
      else (
        (* +++ drain unread bytes from client connection +++ *)
        (match req_body with
        | Some (`Chunked f) ->
            if not !read_chunk_complete then ignore (f ignore)
        | Some _ | None -> ());
        (handle_request [@tailcall]) t conn unconsumed)
  | exception Eof -> Client_connection.close conn
  | exception Parse_error msg ->
      Printf.eprintf "Request parsing error: %s" msg;
      let res = Http.Response.make ~version:`HTTP_1_1 ~status:`Bad_request () in
      write_response conn (res, None)
  | exception exn ->
      Printf.eprintf "Unhandled exception: %s" (Printexc.to_string exn);
      let res =
        Http.Response.make ~version:`HTTP_1_1 ~status:`Internal_server_error ()
      in
      write_response conn (res, None)

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
        let conn =
          Client_connection.
            {
              flow;
              addr;
              switch = sw;
              ic = (flow :> Eio.Flow.read);
              oc = (flow :> Eio.Flow.write);
            }
        in
        handle_request t conn (ref Cstruct.empty))
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

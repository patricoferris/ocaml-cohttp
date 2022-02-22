open Eio.Std

type handler = Request.t -> Response.t
type middleware = handler -> handler

type t = {
  socket_backlog : int;
  domains : int;
  port : int;
  request_handler : handler;
  stopped : bool Atomic.t;
}

type client_conn = {
  flow : < Eio.Flow.two_way ; Eio.Flow.close >;
  reader : Eio.Buf_read.t;
  response_buf : Buffer.t;
  req : Request.t;
}

let close t = Eio.Flow.close t.flow
let stop t = ignore @@ Atomic.compare_and_set t.stopped false true

let domain_count =
  match Sys.getenv_opt "COHTTP_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

(* https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
let write_chunked (client_conn : client_conn) chunk_writer =
  let extensions exts =
    let buf = Buffer.create 0 in
    List.iter
      (fun { Chunk.name; value } ->
        let v =
          match value with None -> "" | Some v -> Printf.sprintf "=%s" v
        in
        Printf.sprintf ";%s%s" name v |> Buffer.add_string buf)
      exts;
    Buffer.contents buf
  in
  let write = function
    | Chunk.Chunk { size; data; extensions = exts } ->
        let buf =
          Printf.sprintf "%X%s\r\n%s\r\n" size (extensions exts)
            (Cstruct.to_string data)
        in
        Eio.Flow.copy_string buf client_conn.flow
    | Chunk.Last_chunk exts ->
        let buf = Printf.sprintf "%X%s\r\n" 0 (extensions exts) in
        Eio.Flow.copy_string buf client_conn.flow
  in
  chunk_writer write

let write_response (client_conn : client_conn)
    { Response.version; body; status; headers } =
  Buffer.clear client_conn.response_buf;
  let buf = client_conn.response_buf in
  let version = Version.to_string version in
  let status = Http.Status.to_string status in
  Buffer.add_string buf version;
  Buffer.add_string buf " ";
  Buffer.add_string buf status;
  Buffer.add_string buf "\r\n";
  headers
  |> Http.Header.clean_dup
  |> Http.Header.iter (fun k v ->
         Buffer.add_string buf k;
         Buffer.add_string buf ": ";
         Buffer.add_string buf v;
         Buffer.add_string buf "\r\n");
  Buffer.add_string buf "\r\n";
  match body with
  | String s ->
      Buffer.add_string buf s;
      Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow
  | Custom writer ->
      Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow;
      writer (client_conn.flow :> Eio.Flow.sink)
  | Chunked chunk_writer ->
      Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow;
      write_chunked client_conn chunk_writer
  | Empty -> Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow

let rec handle_request (t : t) (client_conn : client_conn) : unit =
  match Request.parse_with client_conn.req with
  | () -> (
      let res = t.request_handler client_conn.req in
      write_response client_conn res;
      if Atomic.get t.stopped then close client_conn
      else
        match
          ( Http.Header.get client_conn.req.headers "connection",
            client_conn.req.version )
        with
        | Some "keep-alive", _ | _, Version.HTTP_1_1 ->
            (* Drain unread bytes from client connection before
               reading another request. *)
            (* if not client_conn.req.read_complete then *)
            (*   match Http.Header.get_transfer_encoding (Request.headers req) with *)
            (*   | Http.Transfer.Fixed _ -> ignore @@ Request.read_fixed req *)
            (*   | Http.Transfer.Chunked -> ignore @@ Request.read_chunk req ignore *)
            (*   | _ -> () *)
            (* else (); *)
            handle_request t client_conn
        | _, Version.HTTP_1_0 -> close client_conn)
  | exception End_of_file -> close client_conn
  | exception Parser.Parse_failure msg ->
      Printf.eprintf "\nRequest parsing error: %s%!" msg;
      write_response client_conn Response.bad_request;
      close client_conn
  | exception exn ->
      Printf.eprintf "\nUnhandled exception: %s%!" (Printexc.to_string exn);
      write_response client_conn Response.internal_server_error;
      close client_conn

let run_domain (t : t) ssock =
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  Switch.run (fun sw ->
      while not (Atomic.get t.stopped) do
        Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error
          (fun ~sw:_ flow _ ->
            let reader = Eio.Buf_read.of_flow ~max_size:(4096 * 5) flow in
            let client_conn =
              {
                flow;
                reader;
                response_buf = Buffer.create 512;
                req = Request.create reader;
              }
            in
            handle_request t client_conn)
      done)

let create ?(socket_backlog = 5) ?(domains = domain_count) ~port request_handler
    =
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

let not_found : handler = fun (_ : Request.t) -> Response.not_found

open Eio.Std

type handler = Request.t -> Response.t option
type middleware = handler -> handler

type t = {
  socket_backlog : int;
  domains : int;
  port : int;
  request_handler : handler;
  closed : bool Atomic.t;
}

(* TODO also close connections? *)
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

(* https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
let write_chunked (client_conn : Client_connection.t) chunk_writer =
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

let write_response (client_conn : Client_connection.t) { Response.res; body } =
  let buf = Buffer.create 1024 in
  let version = Http.Response.version res |> Http.Version.to_string in
  let status = Http.Response.status res |> Http.Status.to_string in
  Buffer.add_string buf version;
  Buffer.add_string buf " ";
  Buffer.add_string buf status;
  Buffer.add_string buf "\r\n";
  let headers = Http.Response.headers res in
  let content_length = "content-length" in
  (match body with
  | `String b ->
      Http.Header.add_unless_exists headers content_length
        (string_of_int @@ String.length b)
  | `Chunked _ -> Http.Header.remove headers content_length
  | `Custom _ -> headers
  | `None -> Http.Header.add_unless_exists headers content_length "0")
  |> Http.Header.clean_dup
  |> Http.Header.iter (fun k v ->
         Buffer.add_string buf k;
         Buffer.add_string buf ": ";
         Buffer.add_string buf v;
         Buffer.add_string buf "\r\n");
  Buffer.add_string buf "\r\n";
  match body with
  | `String s ->
      Buffer.add_string buf s;
      Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow
  | `Custom writer ->
      Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow;
      writer (client_conn.flow :> Eio.Flow.sink)
  | `Chunked chunk_writer ->
      Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow;
      write_chunked client_conn chunk_writer
  | `None -> Eio.Flow.copy_string (Buffer.contents buf) client_conn.flow

let rec handle_request (t : t) (conn : Client_connection.t) : unit =
  match Reader.parse conn.reader Parser.request with
  | req -> (
      let req = Request.{ req; reader = conn.reader; read_complete = false } in
      match t.request_handler req with
      | Some { Response.res; body } -> (
          let keep_alive = Request.is_keep_alive req in
          let response_headers =
            Http.Header.add_unless_exists
              (Http.Response.headers res)
              "connection"
              (if keep_alive then "keep-alive" else "close")
          in
          let res = { res with headers = response_headers } in
          write_response conn { Response.res; body };
          match (keep_alive, Atomic.get t.closed) with
          | _, true | false, _ -> Client_connection.close conn
          | true, false ->
              (* Drain unread bytes from client connection before
                 reading another request. *)
              if not req.read_complete then
                match
                  Http.Header.get_transfer_encoding (Request.headers req)
                with
                | Http.Transfer.Fixed _ -> ignore @@ Request.read_fixed req
                | Http.Transfer.Chunked ->
                    ignore @@ Request.read_chunk req ignore
                | _ -> ()
              else ();
              (handle_request [@tailcall]) t conn)
      | None ->
          Printf.eprintf "Request not handled%!";
          write_response conn Response.internal_server_error)
  | exception Parser.Eof -> Client_connection.close conn
  | exception Reader.Parse_error msg ->
      Printf.eprintf "Request parsing error: %s" msg;
      write_response conn Response.bad_request
  | exception exn ->
      Printf.eprintf "Unhandled exception: %s" (Printexc.to_string exn);
      write_response conn Response.internal_server_error

let run_accept_loop (t : t) sw env =
  let net = Eio.Stdenv.net env in
  let sockaddr = `Tcp (Eio.Net.Ipaddr.V4.loopback, t.port) in
  let ssock =
    Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog:t.socket_backlog
      ~sw net sockaddr
  in
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  while not (Atomic.get t.closed) do
    Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error
    @@ fun ~sw flow addr ->
    let client_conn =
      {
        Client_connection.flow;
        addr;
        switch = sw;
        reader = Reader.create (flow :> Eio.Flow.source);
      }
    in
    handle_request t client_conn
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

(* Basic handlers *)

let not_found : handler = fun (_ : Request.t) -> Some Response.not_found

(* Handler combinators *)

let join h1 h2 req = match h1 req with Some _ as res -> res | None -> h2 req

module Infix = struct
  let ( >>? ) = join
end

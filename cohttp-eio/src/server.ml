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
let write_chunked faraday chunk_writer =
  let write_extensions exts =
    List.iter
      (fun { Chunk.name; value } ->
        Faraday.write_char faraday ';';
        Faraday.write_string faraday name;
        Option.iter
          (fun v ->
            let v = Printf.sprintf "=%s" v in
            Faraday.write_string faraday v)
          value)
      exts
  in
  let write = function
    | Chunk.Chunk { size; data; extensions } ->
        let size = Printf.sprintf "%X" size in
        Faraday.write_string faraday size;
        write_extensions extensions;
        Faraday.write_string faraday "\r\n";
        Faraday.write_bigstring faraday data.buffer;
        Faraday.write_string faraday "\r\n"
    | Chunk.Last_chunk extensions ->
        let size = Printf.sprintf "%X" 0 in
        Faraday.write_string faraday size;
        write_extensions extensions;
        Faraday.write_string faraday "\r\n"
  in
  chunk_writer write

let write_response (client_conn : Client_connection.t) { Response.res; body } =
  let faraday = Faraday.create Reader.default_io_buffer_size in
  let serialize () =
    let status_line =
      let version = Http.Response.version res |> Http.Version.to_string in
      let status = Http.Response.status res |> Http.Status.to_int in
      let status_phrase = Http.Status.reason_phrase_of_code status in
      Printf.sprintf "%s %d %s\r\n" version status status_phrase
    in
    Faraday.write_string faraday status_line;
    let headers = Http.Response.headers res in
    let headers =
      (*--- Don't cache set-cookie headers in browsers and proxies. ---*)
      if Http.Header.mem headers "set-cookie" then
        Http.Header.add headers "Cache-Control" {|no-cache="Set-Cookie"|}
      else headers
    in
    let headers =
      let hdr = "content-length" in
      match body with
      | `String cs ->
          Http.Header.add_unless_exists headers hdr
            (string_of_int @@ Cstruct.length cs)
      | `Chunked _ -> Http.Header.remove headers hdr
      | `Custom _ -> headers
      | `None -> Http.Header.add_unless_exists headers hdr "0"
    in
    let headers = Http.Header.clean_dup headers in
    Http.Header.iter
      (fun name v ->
        let hdr = Printf.sprintf "%s: %s\r\n" name v in
        Faraday.write_string faraday hdr)
      headers;
    Faraday.write_string faraday "\r\n";
    (match body with
    | `String (cs : Cstruct.t) ->
        Faraday.write_bigstring faraday ~off:cs.off ~len:cs.len
          (Cstruct.to_bigarray cs)
    | `Custom f -> f faraday
    | `Chunked chunk_writer -> write_chunked faraday chunk_writer
    | `None -> ());
    if not (Faraday.is_closed faraday) then Faraday.close faraday
  in
  let rec write () =
    match Faraday.operation faraday with
    | `Writev iovecs ->
        let iovecs =
          List.map
            (fun { Faraday.buffer; off; len } ->
              Cstruct.of_bigarray ~off ~len buffer)
            iovecs
        in
        let source = Eio.Flow.cstruct_source iovecs in
        Eio.Flow.copy source client_conn.oc;
        (write [@tailcall]) ()
    | `Yield ->
        Eio.Std.Fibre.yield ();
        (write [@tailcall]) ()
    | `Close -> ()
  in
  Eio.Std.Fibre.both serialize write

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
          match keep_alive with
          | false -> Client_connection.close conn
          | true ->
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
    handle_request t
      {
        Client_connection.flow;
        addr;
        switch = sw;
        reader = Reader.create (flow :> Eio.Flow.read);
        oc = (flow :> Eio.Flow.write);
      }
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

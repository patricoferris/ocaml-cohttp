open Eio.Std

module Client_connection = struct
  type t = {
    flow : < Eio.Flow.two_way ; Eio.Flow.close >;
    switch : Eio.Std.Switch.t;
    addr : Eio.Net.Sockaddr.t;
    ic : Reader.t;
    oc : Eio.Flow.write;
  }

  let close t = Eio.Flow.close t.flow
end

module type Request_body_reader = sig
  val read_fixed : unit -> (Cstruct.t, string) result
  val read_chunk : (Body.chunk -> unit) -> (Http.Request.t, string) result
  val reader : Reader.t
  val set_read_complete : unit -> unit
end

type request = Http.Request.t * (module Request_body_reader)

type response = (Http.Response.t * response_body) option
and response_body = [ unit Body.t | `Custom of Faraday.t -> unit ]

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

(** [to_rfc1123 t] converts [t] to a string in a format as defined by RFC 1123. *)
let datetime_to_string (tm : Unix.tm) =
  let weekday =
    match tm.tm_wday with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | 7 -> "Sun"
    | _ -> assert false
  in
  let month =
    match tm.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> assert false
  in
  Format.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday tm.tm_mday month
    (1900 + tm.tm_year) tm.tm_hour tm.tm_min tm.tm_sec

let write_chunked _read_chunk = ()

let write_response (client_conn : Client_connection.t)
    (res, (body : response_body)) =
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
    let headers =
      let date = Unix.time () |> Unix.gmtime in
      Http.Header.add_unless_exists headers "Date" (datetime_to_string date)
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
    | `Chunked read_chunk -> write_chunked read_chunk
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

let make_request_body_reader (conn : Client_connection.t) (req : Http.Request.t)
    (read_complete : bool ref) =
  let module M = struct
    let reader = conn.ic

    let read_fixed =
      match Http.Header.get_transfer_encoding req.headers with
      | Http.Transfer.Fixed content_length -> (
          fun () ->
            if !read_complete then Error "End of file"
            else
              let content_length = Int64.to_int content_length in
              try Ok Parser.(parse (fixed_body content_length) conn.ic)
              with e -> Error (Printexc.to_string e))
      | _ -> fun () -> Error "Request is not a fixed content body"

    let read_chunk =
      match Http.Header.get_transfer_encoding req.headers with
      | Http.Transfer.Chunked ->
          let total_read = ref 0 in
          let rec chunk_loop f =
            if !read_complete then Error "End of file"
            else
              let chunk = Parser.(parse (chunk !total_read req) conn.ic) in
              match chunk with
              | `Chunk (data, length, extensions) ->
                  f (Body.Chunk { data; length; extensions });
                  total_read := !total_read + length;
                  (chunk_loop [@tailcall]) f
              | `Last_chunk (extensions, updated_request) ->
                  read_complete := true;
                  f (Body.Last_chunk extensions);
                  Ok updated_request
          in
          chunk_loop
      | _ -> fun _ -> Error "Request is not a chunked request"

    let set_read_complete () = read_complete := true
  end in
  (module M : Request_body_reader)

let make_v1_1_response ?headers ?(status = `OK) body : response =
  let res = Http.Response.make ?headers ~version:`HTTP_1_1 ~status () in
  Some (res, body)

let internal_server_error conn =
  let res =
    Http.Response.make ~version:`HTTP_1_1 ~status:`Internal_server_error ()
  in
  write_response conn (res, `None)

let rec handle_request (t : t) (conn : Client_connection.t) : unit =
  match Parser.(parse request conn.ic) with
  | req -> (
      let read_complete = ref false in
      let req_body_reader = make_request_body_reader conn req read_complete in
      match t.request_handler (req, req_body_reader) with
      | Some (res, res_body) -> (
          let keep_alive = Http.Request.is_keep_alive req in
          let response_headers =
            Http.Header.add_unless_exists
              (Http.Response.headers res)
              "connection"
              (if keep_alive then "keep-alive" else "close")
          in
          let res = { res with headers = response_headers } in
          write_response conn (res, res_body);
          match keep_alive with
          | false -> Client_connection.close conn
          | true ->
              (* Drain unread bytes from client connection before
                 reading another request. *)
              if not !read_complete then
                let module Request_body_reader = (val req_body_reader
                                                    : Request_body_reader)
                in
                match Http.Header.get_transfer_encoding req.headers with
                | Http.Transfer.Fixed _ ->
                    ignore @@ Request_body_reader.read_fixed ()
                | Http.Transfer.Chunked ->
                    ignore @@ Request_body_reader.read_chunk ignore
                | _ -> ()
              else ();
              (handle_request [@tailcall]) t conn)
      | None ->
          Printf.eprintf "Request not handled%!";
          internal_server_error conn)
  | exception Parser.Eof -> Client_connection.close conn
  | exception Parser.Parse_error msg ->
      Printf.eprintf "Request parsing error: %s" msg;
      let res = Http.Response.make ~version:`HTTP_1_1 ~status:`Bad_request () in
      write_response conn (res, `None)
  | exception exn ->
      Printf.eprintf "Unhandled exception: %s" (Printexc.to_string exn);
      internal_server_error conn

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
        ic = Reader.create (flow :> Eio.Flow.read);
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

(* Basic Response *)

let text body =
  let headers =
    Http.Header.init_with "content-type" "text/plain; charset=UTF-8"
  in
  let body = Cstruct.of_string body in
  make_v1_1_response ~headers (`String body)

let html body =
  let headers =
    Http.Header.init_with "content-type" "text/html; charset=UTF-8"
  in
  let body = Cstruct.of_string body in
  make_v1_1_response ~headers (`String body)

(* Basic handlers *)

let not_found : handler =
 fun (_ : request) -> make_v1_1_response ~status:`Not_found `None

(* Handler combinators *)

let join h1 h2 req = match h1 req with Some _ as res -> res | None -> h2 req

module Infix = struct
  let ( >>? ) = join
end

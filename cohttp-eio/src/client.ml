open Eio

type response = Http.Response.t * Reader.t

type body_disallowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  Eio.Stdenv.t ->
  Eio.Switch.t ->
  Eio.Net.Sockaddr.stream ->
  Uri.t ->
  response

type body_allowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  ?body:Body.t ->
  Eio.Stdenv.t ->
  Eio.Switch.t ->
  Eio.Net.Sockaddr.stream ->
  Uri.t ->
  response

(* Request line https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.1 *)
let write_request writer (meth, version, headers, uri, body) =
  Writer.write_string writer (Http.Method.to_string meth);
  Writer.write_char writer ' ';
  Writer.write_string writer (Uri.path_and_query uri);
  Writer.write_char writer ' ';
  Writer.write_string writer (Http.Version.to_string version);
  Writer.write_string writer "\r\n";
  Writer.write_headers writer headers;
  Writer.write_string writer "\r\n";
  Writer.write_body writer body

(* response parser *)

let is_digit = function '0' .. '9' -> true | _ -> false

let status_code =
  let open Reader in
  let+ status = take_while1 is_digit in
  Http.Status.of_int (int_of_string status)

let reason_phrase =
  Reader.take_while (function
    | '\x21' .. '\x7E' | '\t' | ' ' -> true
    | _ -> false)

(* https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.2 *)
let response reader =
  let open Reader in
  match end_of_input reader with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let version = (version <* space) reader in
      let status = (status_code <* space) reader in
      let () = (reason_phrase *> crlf *> return ()) reader in
      let headers = http_headers reader in
      commit reader;
      Http.Response.make ~version ~status ~headers ()

(* Generic HTTP call *)

let call ?(meth = `GET) ?(version = `HTTP_1_1) ?(headers = Http.Header.init ())
    ?(body = Body.Empty) env sw stream uri =
  let sock = Net.connect ~sw (Stdenv.net env) stream in
  let writer = Writer.create (sock :> Flow.sink) in
  Fiber.fork ~sw (fun () -> Writer.run writer);
  write_request writer (meth, version, headers, uri, body);
  Writer.wakeup writer;

  let reader = Reader.create 0x1000 (sock :> Flow.source) in
  let response = response reader in
  (response, reader)

(*  HTTP Calls with Body Disallowed *)

let get ?version ?headers env sw stream uri =
  call ~meth:`GET ?version ?headers env sw stream uri

let head ?version ?headers env sw stream uri =
  call ~meth:`HEAD ?version ?headers env sw stream uri

let delete ?version ?headers env sw stream uri =
  call ~meth:`DELETE ?version ?headers env sw stream uri

(*  HTTP Calls with Body Allowed *)

let post ?version ?headers ?body env sw stream uri =
  call ~meth:`POST ?version ?headers ?body env sw stream uri

let put ?version ?headers ?body env sw stream uri =
  call ~meth:`PUT ?version ?headers ?body env sw stream uri

let patch ?version ?headers ?body env sw stream uri =
  call ~meth:`PATCH ?version ?headers ?body env sw stream uri

(* Response Body *)
let read_fixed ((response, reader) : Http.Response.t * Reader.t) =
  Body.read_fixed reader response.headers

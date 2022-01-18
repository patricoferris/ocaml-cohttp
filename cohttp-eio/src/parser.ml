open Angstrom

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let space = char '\x20'
let htab = char '\t'
let ows = skip_many (space <|> htab)
let optional x = option None (x >>| Option.some)
let vchar = satisfy (function '\x21' .. '\x7E' -> true | _ -> false)
let digit = satisfy (function '0' .. '9' -> true | _ -> false)
let crlf = string_ci "\r\n" <?> "[crlf]"

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
let headers =
  let header_field =
    let* header_name = token <* char ':' <* ows >>| String.lowercase_ascii in
    let+ header_value =
      let field_content =
        let c2 =
          optional
            (let+ c1 = skip_many1 (space <|> htab) *> vchar in
             Format.sprintf " %c" c1)
          >>| function
          | Some s -> s
          | None -> ""
        in
        lift2 (fun c1 c2 -> Format.sprintf "%c%s" c1 c2) vchar c2
      in
      many field_content >>| String.concat "" <* crlf <* commit
    in
    (header_name, header_value)
  in
  many header_field <* commit >>| Http.Header.of_list_rev

(*-- request-line = method SP request-target SP HTTP-version CRLF HTTP headers *)
let[@warning "-3"] request =
  let* meth = token >>| Http.Method.of_string <* space in
  let* resource = take_while1 (fun c -> c != ' ') <* space in
  let* version =
    let* v = string "HTTP/1." *> digit <* crlf in
    match v with
    | '1' -> return `HTTP_1_1
    | '0' -> return `HTTP_1_0
    | _ -> fail (Format.sprintf "Invalid HTTP version: %c" v)
  in
  let+ headers = headers <* commit in
  {
    Http.Request.headers;
    meth;
    scheme = None;
    resource;
    version;
    encoding = Http.Header.get_transfer_encoding headers;
  }

(* Chunked encoding parser *)

let hex_digit = function
  | '0' .. '9' -> true
  | 'a' .. 'f' -> true
  | 'A' .. 'F' -> true
  | _ -> false

let quoted_pair = char '\\' *> (space <|> htab <|> vchar)

(*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
let qdtext =
  htab
  <|> space
  <|> char '\x21'
  <|> satisfy (function
        | '\x23' .. '\x5B' -> true
        | '\x5D' .. '\x7E' -> true
        | _ -> false)

(*-- quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
let quoted_string =
  let dquote = char '"' in
  let+ chars = dquote *> many_till (qdtext <|> quoted_pair) dquote <* dquote in
  String.of_seq @@ List.to_seq chars

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 --*)
let chunk_exts =
  let chunk_ext_name = token in
  let chunk_ext_val = quoted_string <|> token in
  many
    (lift2
       (fun name value : Body.chunk_extension -> { name; value })
       (char ';' *> chunk_ext_name)
       (optional (char '=' *> chunk_ext_val)))

let chunk_size =
  let* sz = take_while1 hex_digit in
  try return (Format.sprintf "0x%s" sz |> int_of_string)
  with _ -> fail (Format.sprintf "Invalid chunk_size: %s" sz)

(* Be strict about headers allowed in trailer headers to minimize security
   issues, eg. request smuggling attack -
   https://portswigger.net/web-security/request-smuggling
   Allowed headers are defined in 2nd paragraph of
   https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.2 *)
let is_trailer_header_allowed h =
  match String.lowercase_ascii h with
  | "transfer-encoding" | "content-length" | "host"
  (* Request control headers are not allowed. *)
  | "cache-control" | "expect" | "max-forwards" | "pragma" | "range" | "te"
  (* Authentication headers are not allowed. *)
  | "www-authenticate" | "authorization" | "proxy-authenticate"
  | "proxy-authorization"
  (* Cookie headers are not allowed. *)
  | "cookie" | "set-cookie"
  (* Response control data headers are not allowed. *)
  | "age" | "expires" | "date" | "location" | "retry-after" | "vary" | "warning"
  (* Headers to process the payload are not allowed. *)
  | "content-encoding" | "content-type" | "content-range" | "trailer" ->
      false
  | _ -> true

(* Request indiates which headers will be sent in chunk trailer part by
   specifying the headers in comma separated value in 'Trailer' header. *)
let request_trailer_headers (req : Http.Request.t) =
  match Http.Header.get req.headers "Trailer" with
  | Some v -> List.map String.trim @@ String.split_on_char ',' v
  | None -> []

(* Chunk decoding algorithm is explained at
   https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3 *)
let chunk (total_read : int) (req : Http.Request.t) =
  let* sz = chunk_size in
  match sz with
  | sz when sz > 0 ->
      let* extensions = chunk_exts <* crlf in
      let* data = take_bigstring sz <* crlf >>| Cstruct.of_bigarray in
      return (Body.Chunk { data; length = sz; extensions })
  | 0 ->
      let* extensions = chunk_exts <* crlf in
      (* Read trailer headers if any and append those to request headers.

         Only headers names appearing in 'Trailer' request headers and "allowed" trailer
         headers are appended to request.

         The spec at https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3
         specifies that 'Content-Length' and 'Transfer-Encoding' headers must be
         updated. *)
      let* trailer_headers = headers <* crlf <* commit in
      let request_trailer_headers = request_trailer_headers req in
      let request_headers = Http.Request.headers req in
      let trailer_headers =
        List.filter
          (fun (name, _) ->
            List.mem name request_trailer_headers
            && is_trailer_header_allowed name)
          (Http.Header.to_list trailer_headers)
      in
      let request_headers =
        List.fold_left
          (fun h (key, v) -> Http.Header.add h key v)
          request_headers trailer_headers
      in
      (* Remove either just the 'chunked' from Transfer-Encoding header value or
         remove the header entirely if value is empty. *)
      let te_header = "Transfer-Encoding" in
      let request_headers =
        match Http.Header.get request_headers te_header with
        | Some header_value ->
            let new_header_value =
              String.split_on_char ',' header_value
              |> List.map String.trim
              |> List.filter (fun v ->
                     let v = String.lowercase_ascii v in
                     not (String.equal v "chunked"))
              |> String.concat ","
            in
            if String.length new_header_value > 0 then
              Http.Header.replace request_headers te_header new_header_value
            else Http.Header.remove request_headers te_header
        | None -> assert false
      in
      (* Remove 'Trailer' from request headers. *)
      let request_headers = Http.Header.remove request_headers "Trailer" in
      (* Add Content-Length header *)
      let request_headers =
        Http.Header.add request_headers "Content-Length"
          (string_of_int total_read)
      in
      let updated_request = { req with headers = request_headers } in
      return (Body.Last_chunk { extensions; updated_request })
  | sz -> fail (Format.sprintf "Invalid chunk size: %d" sz)

let io_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 in bytes *)

exception Parse_error of string

let parse : 'a Angstrom.t -> #Eio.Flow.read -> Cstruct.t -> Cstruct.t * 'a =
 fun p client_fd unconsumed ->
  let rec loop = function
    | Buffered.Partial k -> (
        let unconsumed_length = Cstruct.length unconsumed in
        if unconsumed_length > 0 then
          loop @@ k (`Bigstring (Cstruct.to_bigarray unconsumed))
        else
          let buf = Cstruct.create io_buffer_size in
          match Eio.Flow.read_into client_fd buf with
          | got ->
              let buf =
                (if got != io_buffer_size then Cstruct.sub buf 0 got else buf)
                |> Cstruct.to_bigarray
              in
              loop (k (`Bigstring buf))
          | exception End_of_file -> loop (k `Eof))
    | Buffered.Done ({ off; len; buf }, x) ->
        let unconsumed =
          if len > 0 then Cstruct.of_bigarray ~off ~len buf else Cstruct.empty
        in
        (unconsumed, x)
    | Buffered.Fail (_, marks, err) ->
        raise (Parse_error (String.concat " > " marks ^ ": " ^ err))
  in
  loop (Buffered.parse p)

let rec read_chunk client_fd unconsumed total_read req f =
  let unconsumed, chunk = parse (chunk total_read req) client_fd unconsumed in
  match chunk with
  | Body.Chunk x as c ->
      f c;
      let total_read = total_read + x.length in
      (read_chunk [@tailcall]) client_fd unconsumed total_read req f
  | Body.Last_chunk _ as c -> f c

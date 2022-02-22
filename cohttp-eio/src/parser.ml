module P : sig
  type 'a t

  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  exception Parse_failure of string

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <?> ) : 'a t -> string -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <* ) : 'a t -> _ t -> 'a t
  val ( *> ) : _ t -> 'b t -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val end_of_input : unit t
  val pos : int t
  val option : 'a -> 'a t -> 'a t
  val peek_char : char t
  val peek_string : int -> string t
  val char : char -> char t
  val satisfy : (char -> bool) -> char t
  val string : string -> string t
  val take_while1 : (char -> bool) -> string t
  val take_while : (char -> bool) -> string t
  val take_bigstring : int -> bigstring t
  val take : int -> string t
  val many : 'a t -> 'a list t
  val many_till : 'a t -> _ t -> 'a list t
  val skip : (char -> bool) -> unit t
  val skip_while : (char -> bool) -> unit t
  val skip_many : 'a t -> unit t
  val parse : Eio.Buf_read.t -> 'a t -> 'a
end = struct
  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type input = { mutable pos : int; rdr : Eio.Buf_read.t }
  type 'a t = input -> 'a

  exception Parse_failure of string

  let[@inline always] return v _ = v
  let[@inline always] fail err _ = Stdlib.raise_notrace (Parse_failure err)

  let[@inline always] ( <?> ) p err inp =
    try p inp with Parse_failure _e -> fail err inp

  let ( >>= ) p f inp =
    let a = p inp in
    f a inp

  let ( let* ) = ( >>= )

  let ( >>| ) p f inp =
    let v = p inp in
    f v

  let ( let+ ) = ( >>| )

  let ( <* ) p q inp =
    let a = p inp in
    let _ = q inp in
    a

  let ( *> ) p q inp =
    let _ = p inp in
    q inp

  let ( <|> ) p q inp =
    let old_pos = inp.pos in
    try p inp
    with Parse_failure _ ->
      inp.pos <- old_pos;
      q inp

  let lift f p = p >>| f

  let lift2 f p q inp =
    let a = p inp in
    let b = q inp in
    f a b

  let pos inp = inp.pos

  let end_of_input inp =
    try
      Eio.Buf_read.ensure inp.rdr 1;
      fail "[end_of_input] not end_of_input" inp
    with End_of_file -> return () inp

  let option : 'a -> 'a t -> 'a t = fun x p -> p <|> return x

  let peek_char inp =
    Eio.Buf_read.ensure inp.rdr 1;
    Cstruct.get_char (Eio.Buf_read.peek inp.rdr) inp.pos

  let peek_string n inp =
    Eio.Buf_read.ensure inp.rdr n;
    Cstruct.to_string (Eio.Buf_read.peek inp.rdr)

  let sprintf = Printf.sprintf

  let char c inp =
    let c' = peek_char inp in
    if c = c' then (
      inp.pos <- inp.pos + 1;
      c)
    else fail (sprintf "[char] expected %C, got %C" c c') inp

  let satisfy f inp =
    let c = peek_char inp in
    if f c then (
      inp.pos <- inp.pos + 1;
      c)
    else fail "[satisfy]" inp

  let string s inp =
    let len = String.length s in
    Eio.Buf_read.ensure inp.rdr len;
    let pos = inp.pos in
    let i = ref 0 in
    while
      !i < len
      && Char.equal
           (Cstruct.get_char (Eio.Buf_read.peek inp.rdr) (pos + !i))
           (String.unsafe_get s !i)
    do
      incr i
    done;
    if len = !i then (
      inp.pos <- inp.pos + len;
      s)
    else fail "[string]" inp

  let count_while inp f =
    let old_pos = inp.pos in
    let pos = ref inp.pos in
    let continue = ref true in
    while !continue do
      Eio.Buf_read.ensure inp.rdr 1;
      let c = Cstruct.get_char (Eio.Buf_read.peek inp.rdr) !pos in
      if f c then incr pos else continue := false
    done;
    !pos - old_pos

  let take_while1 f inp =
    let count = count_while inp f in
    if count < 1 then fail "[take_while1] count is less than 1" inp
    else
      let s =
        Cstruct.to_string (Eio.Buf_read.peek inp.rdr) ~off:inp.pos ~len:count
      in
      inp.pos <- inp.pos + count;
      s

  let take_while f inp =
    let count = count_while inp f in
    if count > 0 then (
      let s =
        Cstruct.to_string (Eio.Buf_read.peek inp.rdr) ~off:inp.pos ~len:count
      in
      inp.pos <- inp.pos + count;
      s)
    else ""

  let take_bigstring : int -> bigstring t =
   fun n inp ->
    Eio.Buf_read.ensure inp.rdr n;
    let s = Cstruct.sub (Eio.Buf_read.peek inp.rdr) inp.pos n in
    inp.pos <- inp.pos + n;
    Cstruct.to_bigarray s

  let take : int -> string t =
   fun n inp ->
    Eio.Buf_read.ensure inp.rdr n;
    let s = Cstruct.to_string (Eio.Buf_read.peek inp.rdr) ~off:inp.pos ~len:n in
    inp.pos <- inp.pos + n;
    s

  let rec many p inp =
    try
      let a = p inp in
      a :: many p inp
    with Parse_failure _ -> []

  let rec many_till p t inp =
    try
      let _ = t inp in
      let a = p inp in
      a :: many_till p t inp
    with Parse_failure _ -> []

  let skip f inp =
    Eio.Buf_read.ensure inp.rdr 1;
    let c = Cstruct.get (Eio.Buf_read.peek inp.rdr) inp.pos in
    if f c then inp.pos <- inp.pos + 1 else fail "[skip]" inp

  let skip_while f inp =
    let count = count_while inp f in
    inp.pos <- inp.pos + count

  let rec skip_many p inp =
    match p inp with _ -> skip_many p inp | exception Parse_failure _ -> ()

  let parse : Eio.Buf_read.t -> 'a t -> 'a =
   fun rdr p ->
    let inp = { pos = 0; rdr } in
    let a = p inp in
    Eio.Buf_read.consume rdr inp.pos;
    a
end

include P

let parse = parse

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
let ows = skip_while (function ' ' | '\t' -> true | _ -> false)
let optional x = option None (x >>| Option.some)
let is_vchar = function '\x21' .. '\x7E' -> true | _ -> false
let vchar = satisfy (function '\x21' .. '\x7E' -> true | _ -> false)
let digit = satisfy (function '0' .. '9' -> true | _ -> false)
let crlf = string "\r\n"

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
let headers =
  let header =
    let* name = token <* char ':' <* ows in
    let+ value =
      take_while (function
        | '\x21' .. '\x7E' -> true (* vchar*)
        | ' ' | '\t' -> true
        | _ -> false)
      <* crlf
    in
    (name, value)
  in
  many header <* crlf >>| Http.Header.of_list

(*-- request-line = method SP request-target SP HTTP-version CRLF HTTP headers *)
let[@warning "-3"] request =
  let request =
    let* meth = token >>| Http.Method.of_string <* space in
    let* resource = take_while1 (fun c -> c != ' ') <* space in
    let* version =
      let* v = string "HTTP/1." *> digit <* crlf in
      match v with
      | '1' -> return `HTTP_1_1
      | '0' -> return `HTTP_1_0
      | _ -> fail (Format.sprintf "Invalid HTTP version: %c" v)
    in
    let+ headers = headers in
    {
      Http.Request.headers;
      meth;
      scheme = None;
      resource;
      version;
      encoding = Http.Header.get_transfer_encoding headers;
    }
  in
  let eof = end_of_input >>| fun () -> Stdlib.raise_notrace End_of_file in
  eof <|> request

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
       (fun name value : Chunk.extension -> { name; value })
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
      return @@ `Chunk (sz, data, extensions)
  | 0 ->
      let* extensions = chunk_exts <* crlf in
      (* Read trailer headers if any and append those to request headers.

         Only headers names appearing in 'Trailer' request headers and "allowed" trailer
         headers are appended to request.

         The spec at https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3
         specifies that 'Content-Length' and 'Transfer-Encoding' headers must be
         updated. *)
      let* trailer_headers = headers in
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
      return @@ `Last_chunk (extensions, updated_request)
  | sz -> fail (Format.sprintf "Invalid chunk size: %d" sz)

let fixed_body content_length =
  if content_length > 0 then
    take_bigstring content_length >>| fun body -> Cstruct.buffer body
  else return Cstruct.empty

type t = {
  reader : Reader.t;
  headers : Http.Header.t;
  version : Http.Version.t;
  meth : Http.Method.t;
  resource : string;
  mutable read_complete : bool;
}

let reader t = t.reader
let headers t = t.headers
let meth t = t.meth
let resource t = t.resource
let version t = t.version

let is_keep_alive t =
  match Http.Header.get t.headers "connection" with
  | Some "close" -> false
  | Some "keep-alive" -> true
  | _ -> Http.Version.(compare t.version `HTTP_1_1) >= 0

open Parser

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let ows = skip_while (function ' ' | '\t' -> true | _ -> false)
let crlf = string "\r\n"
let is_cr = function '\r' -> true | _ -> false
let space = char '\x20'
let p_meth = token <* space >>| Http.Method.of_string
let p_resource = take_while1 (fun c -> c != ' ') <* space

let p_version =
  string "HTTP/1." *> any_char <* crlf >>= function
  | '1' -> return `HTTP_1_1
  | '0' -> return `HTTP_1_0
  | v -> fail (Format.sprintf "Invalid HTTP version: %C" v)

let header =
  lift2
    (fun key value -> (key, value))
    (token <* char ':' <* ows)
    (take_till is_cr <* crlf)

let p_headers =
  let cons x xs = x :: xs in
  fix (fun headers ->
      let _emp = return [] in
      let _rec = lift2 cons header headers in
      peek_char >>= function '\r' -> _emp | _ -> _rec)
  >>| Http.Header.of_list
  <* crlf

let parse reader =
  match end_of_input reader with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let meth = p_meth reader in
      let resource = p_resource reader in
      let version = p_version reader in
      let headers = p_headers reader in
      commit reader;
      { reader; read_complete = false; meth; resource; version; headers }

let read_fixed t =
  if t.read_complete then Error "End of file"
  else
    match Http.Header.get t.headers "content-length" with
    | Some v -> (
        try
          let content_length = int_of_string v in
          let content = Parser.take content_length t.reader in
          Ok content
        with e -> Error (Printexc.to_string e))
    | None -> Error "Request is not a fixed content body"

(* let read_chunk _t = failwith "no implemented" *)
(* match Http.Header.get_transfer_encoding t.headers with *)
(* | Http.Transfer.Chunked -> *)
(*     let total_read = ref 0 in *)
(*     let rec chunk_loop f = *)
(*       if t.read_complete then Error "End of file" *)
(*       else *)
(*         let chunk = Parser.(parse t.reader (chunk !total_read t. extensions) -> *)
(*             f (Chunk.Chunk { size; data; extensions }); *)
(*             total_read := !total_read + size; *)
(*             (chunk_loop [@tailcall]) f *)
(*         | `Last_chunk (extensions, updated_request) -> *)
(*             t.read_complete <- true; *)
(*             f (Chunk.Last_chunk extensions); *)
(*             Ok { t with req = updated_request } *)
(*     in *)
(*     chunk_loop *)
(* | _ -> fun _ -> Error "Request is not a chunked request" *)

type t = {
  mutable version : Version.t;
  headers : Header.t;
  mutable meth : Method.t;
  mutable resource : string;
  reader : Reader.t;
  mutable read_complete : bool;
}

let reader t = t.reader
let headers t = t.headers
let meth t = t.meth
let resource t = t.resource
let version t = t.version
let is_keep_alive t = Header.is_keep_alive t.headers

let create ?(initial_header_len = 15) reader =
  {
    version = Version.HTTP_1_1;
    headers = Header.create initial_header_len;
    meth = Method.Other "";
    reader;
    resource = "";
    read_complete = false;
  }

module P = Parser

let token =
  P.take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let ows = P.skip_while (function ' ' | '\t' -> true | _ -> false)
let crlf = P.string "\r\n"
let is_cr = function '\r' -> true | _ -> false
let space = P.char '\x20'
let p_meth = P.(token <* space >>| Method.of_string)
let p_resource = P.(take_while1 (fun c -> c != ' ') <* space)

let p_version =
  P.(
    string "HTTP/1." *> any_char <* crlf >>= function
    | '1' -> return Version.HTTP_1_1
    | '0' -> return Version.HTTP_1_0
    | v -> fail (Format.sprintf "Invalid HTTP version: %C" v))

let p_header =
  P.(
    lift2
      (fun key value -> (key, value))
      (token <* char ':' <* ows)
      (take_till is_cr <* crlf))

let rec p_headers : Header.t -> unit P.t =
 fun hdrs inp ->
  p_header inp |> Header.add_header hdrs;
  match P.peek_char inp with '\r' -> crlf inp | _ -> p_headers hdrs inp

let parse_into (t : t) =
  Header.clear t.headers;
  match P.end_of_input t.reader with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      t.meth <- p_meth t.reader;
      t.resource <- p_resource t.reader;
      t.version <- p_version t.reader;
      p_headers t.headers t.reader;
      P.commit t.reader;
      t.read_complete <- false

(* let read_fixed t = *)
(*   match Http.Header.get_transfer_encoding t.headers with *)
(*   | Http.Transfer.Fixed content_length -> ( *)
(*       if t.read_complete then Error "End of file" *)
(*       else *)
(*         let content_length = Int64.to_int content_length in *)
(*         try Result.ok @@ Parser.(parse t.reader (fixed_body content_length)) *)
(*         with e -> Error (Printexc.to_string e)) *)
(*   | _ -> Error "Request is not a fixed content body" *)

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

let set_read_complete t = t.read_complete <- true

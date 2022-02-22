type t = {
  mutable version : Version.t;
  mutable headers : Http.Header.t;
  mutable meth : Method.t;
  mutable resource : string;
  reader : Eio.Buf_read.t;
  mutable read_complete : bool;
}

let reader t = t.reader
let headers t = t.headers
let meth t = t.meth
let resource t = t.resource
let version t = t.version

let create reader =
  {
    version = Version.HTTP_1_1;
    headers = Http.Header.init ();
    meth = Method.Other "";
    reader;
    resource = "";
    read_complete = false;
  }

let is_keep_alive t =
  match Http.Header.get t.headers "connection" with
  | Some v when v = "keep-alive" -> true
  | Some _ | _ -> false

let parse_with (r : t) : unit =
  let open Parser in
  let p =
    end_of_input >>= function
    | true -> Stdlib.raise_notrace End_of_file
    | false ->
        let* meth = token <* space in
        let* resource = take_while1 (fun c -> c != ' ') <* space in
        let* version =
          string "HTTP/1." *> digit <* crlf >>= function
          | '1' -> return Version.HTTP_1_1
          | '0' -> return Version.HTTP_1_0
          | v -> fail (Format.sprintf "Invalid HTTP version: %c" v)
        in
        let+ headers = headers in
        r.version <- version;
        r.headers <- headers;
        r.meth <- Method.of_string meth;
        r.resource <- resource;
        r.read_complete <- false
  in
  parse r.reader p

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

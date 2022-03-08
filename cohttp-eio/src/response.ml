type t = {
  headers : Header.t;
  write_buffer : Buffer.t;
  sink : Eio.Flow.sink;
  mutable body : body;
  mutable status : Http.Status.t;
  mutable version : Version.t;
}

and body =
  | String of string
  | Chunked of write_chunk
  | Custom of (Eio.Flow.sink -> unit)
  | Empty

and write_chunk = (Chunk.t -> unit) -> unit

let create ?(version = Version.HTTP_1_1) ?(status = `OK) sink headers body =
  { headers; write_buffer = Buffer.create 1024; sink; body; status; version }

(* Response Details *)

let headers t = t.headers
let status t = t.status
let body t = t.body
let set_body t body = t.body <- body
let set_status t status = t.status <- status
let set_version t version = t.version <- version

let clear t =
  Header.clear t.headers;
  Buffer.clear t.write_buffer

(* https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
let write_chunked flow chunk_writer =
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
        Eio.Flow.copy_string buf flow
    | Chunk.Last_chunk exts ->
        let buf = Printf.sprintf "%X%s\r\n" 0 (extensions exts) in
        Eio.Flow.copy_string buf flow
  in
  chunk_writer write

let write t =
  let version = Version.to_string t.version in
  let status = Http.Status.to_string t.status in
  Buffer.add_string t.write_buffer version;
  Buffer.add_string t.write_buffer " ";
  Buffer.add_string t.write_buffer status;
  Buffer.add_string t.write_buffer "\r\n";
  Header.iter
    (fun k v ->
      Buffer.add_string t.write_buffer k;
      Buffer.add_string t.write_buffer ": ";
      Buffer.add_string t.write_buffer v;
      Buffer.add_string t.write_buffer "\r\n")
    t.headers;
  Buffer.add_string t.write_buffer "\r\n";
  match t.body with
  | String s ->
      Buffer.add_string t.write_buffer s;
      Eio.Flow.copy_string (Buffer.contents t.write_buffer) t.sink
  | Custom writer -> writer t.sink
  | Chunked chunk_writer ->
      Eio.Flow.copy_string (Buffer.contents t.write_buffer) t.sink;
      write_chunked t.sink chunk_writer
  | Empty -> Eio.Flow.copy_string (Buffer.contents t.write_buffer) t.sink

(* Basic Response *)

let text t body =
  Header.add_header t.headers ("content-type", "text/plain; charset=UTF-8");
  Header.add_header t.headers
    ("content-length", string_of_int @@ String.length body);
  t.body <- String body

let html t body =
  Header.add_header t.headers ("content-type", "text/html; charset=UTF-8");
  Header.add_header t.headers
    ("content-length", string_of_int @@ String.length body);
  t.body <- String body

let not_found t =
  t.status <- `Not_found;
  t.body <- Empty

let internal_server_error t =
  t.status <- `Internal_server_error;
  t.body <- Empty

let bad_request t =
  t.status <- `Bad_request;
  t.body <- Empty

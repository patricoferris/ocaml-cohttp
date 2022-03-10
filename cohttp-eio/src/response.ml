type t = {
  headers : Header.t;
  body : body;
  status : Http.Status.t;
  version : Version.t;
}

and body =
  | String of string
  | Chunked of write_chunk
  | Custom of (Eio.Flow.sink -> unit)
  | Empty

and write_chunk = (Chunk.t -> unit) -> unit

let create ?(version = Version.HTTP_1_1) ?(status = `OK)
    ?(headers = Header.init ()) body =
  { headers; body; status; version }

(* Response Details *)

let headers t = t.headers
let status t = t.status
let body t = t.body

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

let write t buffer sink =
  Buffer.clear buffer;
  let version = Version.to_string t.version in
  let status = Http.Status.to_string t.status in
  Buffer.add_string buffer version;
  Buffer.add_string buffer " ";
  Buffer.add_string buffer status;
  Buffer.add_string buffer "\r\n";
  Header.iter
    (fun k v ->
      Buffer.add_string buffer k;
      Buffer.add_string buffer ": ";
      Buffer.add_string buffer v;
      Buffer.add_string buffer "\r\n")
    t.headers;
  Buffer.add_string buffer "\r\n";
  match t.body with
  | String s ->
      Buffer.add_string buffer s;
      Eio.Flow.copy_string (Buffer.contents buffer) sink
  | Custom writer -> writer sink
  | Chunked chunk_writer ->
      Eio.Flow.copy_string (Buffer.contents buffer) sink;
      write_chunked sink chunk_writer
  | Empty -> Eio.Flow.copy_string (Buffer.contents buffer) sink

(* Basic Response *)

let text body =
  let headers = Header.create 2 in
  Header.add_header headers ("content-type", "text/plain; charset=UTF-8");
  Header.add_header headers
    ("content-length", string_of_int @@ String.length body);

  create ~headers (String body)

let html body =
  let headers = Header.create 0 in
  Header.add_header headers ("content-type", "text/html; charset=UTF-8");
  Header.add_header headers
    ("content-length", string_of_int @@ String.length body);
  create ~headers (String body)

let not_found = create ~status:`Not_found Empty
let internal_server_error = create ~status:`Internal_server_error Empty
let bad_request = create ~status:`Bad_request Empty

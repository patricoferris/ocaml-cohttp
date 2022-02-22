type t = {
  headers : Http.Header.t;
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

let create ?(version = Version.HTTP_1_1) ?(headers = Http.Header.init ())
    ?(status = `OK) body =
  { headers; body; status; version }

(* Response Details *)

let headers t = t.headers
let status t = t.status
let body t = t.body

(* Basic Response *)

let text body =
  let headers = Http.Header.init () in
  let headers =
    Http.Header.add_list headers
      [
        ("content-type", "text/plain; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  create ~headers (String body)

let html body =
  let headers = Http.Header.init () in
  let headers =
    Http.Header.add_list headers
      [
        ("content-type", "text/html; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  create ~headers (String body)

let not_found = create ~status:`Not_found Empty
let internal_server_error = create ~status:`Internal_server_error Empty
let bad_request = create ~status:`Bad_request Empty

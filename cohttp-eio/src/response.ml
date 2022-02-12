type t = { res : Http.Response.t; body : body }

and body =
  [ `String of string
  | `Chunked of write_chunk
  | `Custom of Eio.Flow.sink -> unit
  | `None ]

and write_chunk = (Chunk.t -> unit) -> unit

let create ?headers ?(status = `OK) body =
  let res = Http.Response.make ?headers ~version:`HTTP_1_1 ~status () in
  { res; body }

(* Response Details *)

let headers t = t.res.headers
let status t = t.res.status
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
  create ~headers (`String body)

let html body =
  let headers = Http.Header.init () in
  let headers =
    Http.Header.add_list headers
      [
        ("content-type", "text/html; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  create ~headers (`String body)

let not_found = create ~status:`Not_found `None
let internal_server_error = create ~status:`Internal_server_error `None
let bad_request = create ~status:`Bad_request `None

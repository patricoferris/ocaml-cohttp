module Net = Eio.Net
module Stdenv = Eio.Stdenv
module Switch = Eio.Switch
open Cohttp_eio

let conn env sw port resource_path =
  let addr = `Tcp (Net.Ipaddr.V4.loopback, port) in
  let flow = Net.connect ~sw env#net addr in
  let host = ("localhost", Some port) in
  (resource_path, host, flow)

let get env sw port =
  let res =
    Client.get
      ~headers:(Http.Header.of_list [ ("Accept", "application/json") ])
      (conn env sw port) "/get"
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

let post env sw port =
  let content = "hello world!" in
  let content_length = String.length content |> string_of_int in
  let res =
    Client.post
      ~headers:
        (Http.Header.of_list
           [
             ("Accept", "application/json"); ("Content-Length", content_length);
           ])
      ~body:(Body.Fixed content) (conn env sw port) "/post"
  in
  match Client.read_fixed res with Some s -> print_string s | None -> ()

(** Write chunk test.

    Read from text file "chunks.txt" and write each line as a chunk. We add some
    chunk extensions to the first chunk. This is purely for demonstrative effect
    and for testing purposes rather than for any such specific requirement. *)
let post_chunk env sw port =
  let rec body_writer chan chunks f =
    match In_channel.input_line chan with
    | Some data ->
        let extensions =
          if chunks = 0 then
            [
              Body.{ name = "ext1"; value = Some "ext1_v" };
              { name = "ext2"; value = Some "ext2_v" };
              { name = "ext3"; value = None };
            ]
          else []
        in
        let chunk =
          Body.Chunk { size = String.length data; data; extensions }
        in
        f chunk;
        body_writer chan (chunks + 1) f
    | None ->
        let last_chunk = Body.Last_chunk [] in
        f last_chunk
  in
  let trailer_writer f =
    let trailer_headers =
      Http.Header.of_list
        [
          ("Expires", "Wed, 21 Oct 2015 07:28:00 GMT");
          ("Header1", "Header1 value text");
          ("Header2", "Header2 value text");
        ]
    in
    f trailer_headers
  in
  In_channel.with_open_text "chunks.txt" (fun chan ->
      Client.post
        ~headers:
          (Http.Header.of_list
             [
               ("Transfer-Encoding", "chunked");
               ("Content-Type", "text/plain");
               ("Trailer", "Expires, Header1");
             ])
        ~body:
          (Body.Chunked { body_writer = body_writer chan 0; trailer_writer })
        (conn env sw port) "/handle_chunk")
  |> Client.read_fixed
  |> function
  | Some r -> print_string r
  | None -> ()

(* Read chunk and dump to a "client_chunks2.txt" *)
let get_chunk env sw port =
  let write_chunk_to_file flow chunk =
    let data = Format.asprintf "%a\n\n" Body.pp_chunk chunk in
    Eio.Flow.copy_string data flow
  in
  let res = Client.get (conn env sw port) "/get_chunk" in
  let path = Eio.Path.(Stdenv.cwd env / "client_chunks2.txt") in
  Eio.Path.with_open_out ~append:false ~create:(`Or_truncate 0o666) path
    (fun flow ->
      Client.read_chunked res (write_chunk_to_file flow) |> function
      | Some headers ->
          let s = Format.asprintf "%a%!" Http.Header.pp_hum headers in
          Eio.Flow.copy_string s flow
      | None -> ())

let () =
  let port = ref 8080 in
  let t = ref "invalid_uri" in
  Arg.parse
    [
      ("-p", Arg.Set_int port, " Server listening port number(8080 default)");
      ( "-t",
        Arg.Set_string t,
        "Specify test case to execute,('invalid_uri' default)" );
    ]
    ignore "An HTTP/1.1 server";

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  match !t with
  | "get" -> get env sw !port
  | "post" -> post env sw !port
  | "post_chunk" -> post_chunk env sw !port
  | "get_chunk" -> get_chunk env sw !port
  | _ -> print_string "Usage: test-client [get|post]"

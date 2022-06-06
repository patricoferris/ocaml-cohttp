(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.
    All rights reserved.
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.
    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)
module Optional_thunk : sig
  type t

  val none : t
  val some : (unit -> unit) -> t
  val call_if_some : t -> unit
end = struct
  type t = unit -> unit

  let none = Sys.opaque_identity (fun () -> ())

  let some f =
    if f == none then
      failwith
        "Optional_thunk: this function is not representable as a some value";
    f

  let call_if_some t = t ()
end

type t = {
  sink : Eio.Flow.sink;
  buf : Buffer.t;
  mutable wakeup : Optional_thunk.t;
}

let create sink =
  let buf = Buffer.create 0x1000 in
  { sink; buf; wakeup = Optional_thunk.none }

let wakeup t =
  let f = t.wakeup in
  t.wakeup <- Optional_thunk.none;
  Optional_thunk.call_if_some f

let write_string t s = Buffer.add_string t.buf s
let write_char t c = Buffer.add_char t.buf c

let write_headers t headers =
  Http.Header.iter
    (fun k v ->
      write_string t k;
      write_string t ": ";
      write_string t v;
      write_string t "\r\n")
    headers

(* https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
let write_chunked t (chunk_writer : Body.chunk_writer) =
  let write_extensions exts =
    List.iter
      (fun { Body.name; value } ->
        let v =
          match value with None -> "" | Some v -> Printf.sprintf "=%s" v
        in
        write_string t (Printf.sprintf ";%s%s" name v))
      exts
  in
  let write_body = function
    | Body.Chunk { size; data; extensions = exts } ->
        write_string t (Printf.sprintf "%X" size);
        write_extensions exts;
        write_string t "\r\n";
        write_string t data;
        write_string t "\r\n"
    | Last_chunk exts ->
        write_string t "0";
        write_extensions exts;
        write_string t "\r\n"
  in
  chunk_writer.body_writer write_body;
  chunk_writer.trailer_writer (write_headers t);
  write_string t "\r\n"

let write_body t body =
  match body with
  | Body.Fixed s -> write_string t s
  | Chunked chunk_writer -> write_chunked t chunk_writer
  | Custom f ->
      wakeup t;
      f (t.sink :> Eio.Flow.sink)
  | Empty -> ()

let run t =
  let rec loop () =
    if Buffer.length t.buf > 0 then (
      Eio.Flow.copy_string (Buffer.contents t.buf) t.sink;
      Buffer.clear t.buf;
      loop ())
    else t.wakeup <- Optional_thunk.some loop
  in
  loop ()

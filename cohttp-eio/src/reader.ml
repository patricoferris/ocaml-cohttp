(* Based on https://github.com/inhabitedtype/angstrom/blob/master/lib/buffering.ml *)
type t = {
  mutable buf : Bigstringaf.t;
  mutable off : int;
  mutable len : int;
  source : Eio.Flow.source;
  buffer_size : int;
}

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let default_io_buffer_size = 4096

let create ?(buffer_size = default_io_buffer_size) source =
  assert (buffer_size > 0);
  let buf = Bigstringaf.create buffer_size in
  let off = 0 in
  let len = 0 in
  { buf; off; len; source; buffer_size }

let source t = t.source
let buffer_size t = t.buffer_size
let offset t = t.off
let length t = t.len
let buffer t = t.buf

let grow t size =
  let extra_len =
    let x = size / t.buffer_size in
    let rem = size mod t.buffer_size in
    let x = if rem > 0 then x + 1 else x in
    x * t.buffer_size
  in
  let new_buf = Bigstringaf.create (t.len + extra_len) in
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0 ~len:t.len;
  t.buf <- new_buf;
  t.off <- 0;
  extra_len

let consume t n =
  assert (t.len >= n);
  t.off <- t.off + n;
  t.len <- t.len - n;
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0 ~len:t.len;
  t.off <- 0

let fill t size =
  let to_read = grow t size in
  let write_off = t.off + t.len in
  let buf = Cstruct.of_bigarray ~off:write_off ~len:to_read t.buf in
  let got = Eio.Flow.read t.source buf in
  t.len <- t.len + got

exception Parse_error of string

let parse t p =
  let open Angstrom in
  let rec loop = function
    | Unbuffered.Partial k ->
        fill t t.buffer_size;
        loop (k.continue t.buf ~off:t.off ~len:t.len Unbuffered.Incomplete)
    | Unbuffered.Done (len, a) ->
        if len > 0 then consume t len;
        a
    | Unbuffered.Fail (len, marks, err) ->
        if len > 0 then consume t len;
        raise (Parse_error (String.concat " > " marks ^ ": " ^ err))
  in
  loop (Unbuffered.parse p)

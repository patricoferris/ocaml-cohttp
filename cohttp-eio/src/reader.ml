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

let default_io_buffer_size = 512

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
  let buf = Cstruct.of_bigarray t.buf ~off:write_off ~len:to_read in
  let got = Eio.Flow.read t.source buf in
  t.len <- t.len + got

(* Based on https://github.com/inhabitedtype/angstrom/blob/master/lib/buffering.ml *)
type t = {
  mutable buf : Bigstringaf.t;
  mutable off : int;
  mutable len : int;
  ic : Eio.Flow.read;
  bufsize : int;
}

let default_io_buffer_size = 4096
let writable_space t = Bigstringaf.length t.buf - t.len
let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)
let write_pos t = t.off + t.len

let create ?(bufsize = default_io_buffer_size) flow =
  let buf = Bigstringaf.create bufsize in
  let off = 0 in
  let len = 0 in
  { buf; off; len; ic = (flow :> Eio.Flow.read); bufsize }

let compress t =
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0 ~len:t.len;
  t.off <- 0

let grow t to_copy =
  let old_len = Bigstringaf.length t.buf in
  let new_len = ref old_len in
  let space = writable_space t in
  while space + !new_len - old_len < to_copy do
    new_len := 3 * !new_len / 2
  done;
  let new_buf = Bigstringaf.create !new_len in
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0 ~len:t.len;
  t.buf <- new_buf;
  t.off <- 0

let ensure t to_copy =
  if trailing_space t < to_copy then
    if writable_space t >= to_copy then compress t else grow t to_copy

let shift t n =
  assert (t.len >= n);
  t.off <- t.off + n;
  t.len <- t.len - n

let feed_input t =
  ensure t t.bufsize;
  let buf = Cstruct.of_bigarray ~off:(write_pos t) ~len:t.bufsize t.buf in
  match Eio.Flow.read_into t.ic buf with
  | got ->
      t.len <- t.len + got;
      (t.buf, t.off, t.len)
  | exception End_of_file -> (t.buf, t.off, 0)

let ensure_input t len =
  if t.len < len then
    let continue = ref true in
    while !continue do
      let _, _, got = feed_input t in
      continue := if got > 0 && t.len < len then true else false
    done

let read_into t ~off ~len buf =
  ensure_input t len;
  let len = if t.len < len then t.len else len in
  if len > 0 then (
    Bigstringaf.unsafe_blit t.buf ~src_off:t.off buf ~dst_off:off ~len;
    shift t len);
  len

let read_char t =
  ensure_input t 1;
  if t.len >= 1 then (
    let c = Bigstringaf.unsafe_get t.buf t.off in
    shift t 1;
    c)
  else raise End_of_file

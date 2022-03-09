type t = {
  source : Eio.Flow.source;
  mutable buf : Bigstringaf.t;
  mutable off : int;
  mutable len : int;
  mutable eof_seen : bool;
}

let create len source =
  assert (len > 0);
  let buf = Bigstringaf.create len in
  { source; buf; off = 0; len = 0; eof_seen = false }

let length t = t.len
let writable_space t = Bigstringaf.length t.buf - t.len
let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)

let compress t =
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0 ~len:t.len;
  t.off <- 0

let grow t to_copy =
  let old_len = Bigstringaf.length t.buf in
  let new_len = ref old_len in
  let space = writable_space t in
  while space + !new_len - old_len < to_copy do
    new_len := 3 * (!new_len + 1) / 2
  done;
  let new_buf = Bigstringaf.create !new_len in
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0 ~len:t.len;
  t.buf <- new_buf;
  t.off <- 0

let adjust_buffer t to_read =
  if trailing_space t < to_read then
    if writable_space t < to_read then grow t to_read else compress t

let consume t n =
  assert (n >= 0);
  t.off <- t.off + n;
  t.len <- t.len - n

let fill t to_read =
  if t.eof_seen then 0
  else (
    adjust_buffer t to_read;
    let off = t.off + t.len in
    let len = trailing_space t in
    match Eio.Flow.read t.source (Cstruct.of_bigarray t.buf ~off ~len) with
    | got ->
        t.len <- t.len + got;
        got
    | exception End_of_file ->
        t.eof_seen <- true;
        0)

let unsafe_get t off = Bigstringaf.unsafe_get t.buf (t.off + off)
let buffer t = Cstruct.of_bigarray t.buf ~off:t.off ~len:t.len

exception Parse_error of string

let parse t p =
  let open Angstrom in
  let rec loop = function
    | Unbuffered.Partial k ->
        consume t k.committed;
        let got = fill t 128 in
        let more =
          if got = 0 then Unbuffered.Complete else Unbuffered.Incomplete
        in
        loop (k.continue t.buf ~off:t.off ~len:t.len more)
    | Unbuffered.Done (len, a) ->
        consume t len;
        a
    | Unbuffered.Fail (len, marks, err) ->
        consume t len;
        raise (Parse_error (String.concat " > " marks ^ ": " ^ err))
  in
  loop (Unbuffered.parse p)

type 'a t = Reader.t -> 'a

exception Parse_failure of string

let return v _ = v
let fail err _ = Stdlib.raise_notrace (Parse_failure err)
let commit inp = Reader.commit inp
let ( <?> ) p err inp = try p inp with Parse_failure _e -> fail err inp

let ( >>= ) p f inp =
  let a = p inp in
  f a inp

let ( let* ) = ( >>= )

let ( >>| ) p f inp =
  let v = p inp in
  f v

let ( let+ ) = ( >>| )

let ( <* ) p q inp =
  let a = p inp in
  let _ = q inp in
  a

let ( *> ) p q inp =
  let _ = p inp in
  q inp

let ( <|> ) p q inp =
  let old_pos = Reader.pos inp in
  let old_committed = Reader.committed_bytes inp in
  try p inp
  with Parse_failure _ as ex ->
    if old_committed < Reader.committed_bytes inp then raise_notrace ex
    else (
      inp.pos <- old_pos;
      q inp)

let lift f p = p >>| f

let lift2 f p q inp =
  let a = p inp in
  let b = q inp in
  f a b

let rec ensure inp len =
  if Reader.(length inp < pos inp + len) then
    let got = Reader.fill inp len in
    if got = 0 then raise_notrace End_of_file else ensure inp len

let pos inp = Reader.pos inp

let end_of_input inp =
  try
    ensure inp 1;
    false
  with End_of_file -> true

let option : 'a -> 'a t -> 'a t = fun x p -> p <|> return x

let peek_char inp =
  let open Reader in
  if pos inp < length inp then unsafe_get inp (pos inp)
  else (
    ensure inp 1;
    unsafe_get inp inp.pos)

let peek_string n inp =
  try
    ensure inp n;
    Reader.substring inp ~off:inp.pos ~len:n
  with End_of_file -> fail "[peek_string] not enough input" inp

let sprintf = Printf.sprintf

let char c inp =
  let c' = peek_char inp in
  if c = c' then Reader.incr_pos inp
  else fail (sprintf "[char] expected %C, got %C" c c') inp

let any_char inp =
  ensure inp 1;
  let c = Reader.unsafe_get inp inp.pos in
  Reader.incr_pos inp;
  c

let satisfy f inp =
  let c = peek_char inp in
  if f c then (
    Reader.incr_pos inp;
    c)
  else fail "[satisfy]" inp

let string s inp =
  let len = String.length s in
  ensure inp len;
  let pos = pos inp in
  let i = ref 0 in
  while
    !i < len
    && Char.equal (Reader.unsafe_get inp (pos + !i)) (String.unsafe_get s !i)
  do
    incr i
  done;
  if len = !i then Reader.incr_pos ~n:len inp else fail "[string]" inp

let count_while inp f =
  let i = ref 0 in
  let continue = ref true in
  while !continue do
    try
      ensure inp (!i + 1);
      let c = Reader.(unsafe_get inp (pos inp + !i)) in
      if f c then incr i else continue := false
    with End_of_file -> continue := false
  done;
  !i

let take_while1 f inp =
  let count = count_while inp f in
  if count < 1 then fail "[take_while1] count is less than 1" inp
  else
    let s = Reader.(substring inp ~off:(pos inp) ~len:count) in
    Reader.incr_pos ~n:count inp;
    s

let take_while f inp =
  let count = count_while inp f in
  if count > 0 then (
    let s = Reader.(substring inp ~off:(pos inp) ~len:count) in
    Reader.incr_pos ~n:count inp;
    s)
  else ""

let take_bigstring : int -> Bigstringaf.t t =
 fun n inp ->
  try
    ensure inp n;
    let s = Reader.(copy inp ~off:(pos inp) ~len:n) in
    Reader.incr_pos ~n inp;
    s
  with End_of_file -> fail "[take_bigstring] not enough input" inp

let take : int -> string t =
 fun n inp ->
  try
    ensure inp n;
    let s = Reader.(substring inp ~off:(pos inp) ~len:n) in
    Reader.incr_pos ~n inp;
    s
  with End_of_file -> fail "[take] not enough input" inp

let take_till f = take_while (fun c -> not (f c))

let rec many : 'a t -> 'a list t =
 fun p inp ->
  try
    let a = p inp in
    a :: many p inp
  with Parse_failure _ | End_of_file -> []

let skip f inp =
  ensure inp 1;
  let c = Reader.(unsafe_get inp (pos inp)) in
  if f c then Reader.incr_pos inp else fail "[skip]" inp

let skip_while f inp =
  let count = count_while inp f in
  Reader.incr_pos ~n:count inp

let rec skip_many p inp =
  match p inp with _ -> skip_many p inp | exception Parse_failure _ -> ()

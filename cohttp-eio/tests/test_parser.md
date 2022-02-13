## Test dependencies

```ocaml
# #require "cohttp-eio";;
# #require "bigstringaf";;
# #require "fmt";;
```

## Prelude: test helpers and pretty printers

```ocaml
open Cohttp_eio
module P = Private.Parser
module R = Reader

type 'a t = 
{ reader_len: int
; pos : int
; result : 'a
; reader: Reader.t
}

let pp = 
  let fields = 
    [ Fmt.field "reader_len " (fun x -> x.reader_len) Fmt.int
    ; Fmt.field "pos        " (fun x -> x.pos)        Fmt.int
    ; Fmt.field "result     " (fun x -> x.result)     Fmt.string
    ]
  in
  Fmt.braces @@ Fmt.record ~sep:Fmt.semi fields

let pp_bigstring fmt bs = Format.fprintf fmt "%s" (Bigstringaf.to_string bs)
let create_reader s = R.create @@ Eio.Flow.string_source s

let parse ?rdr p s = 
  let p = P.(lift2 (fun a pos -> (a, pos)) p pos) in
  let rdr = 
    match rdr with
    | Some v -> v 
    | None -> create_reader s 
  in
  let (a, pos)  = P.parse rdr p in
  { reader_len  = R.length rdr
  ; pos
  ; result      = a
  ; reader      = rdr
  }
```
```ocaml
# #install_printer pp_bigstring;;
# #install_printer pp;;
```

## Basic: return, fail
```ocaml
let p1 = P.return "hello"
let p2 = P.fail "parse error"
```

```ocaml
# parse p1 "";;
- : string t = {reader_len : 0;
                pos        : 0;
                result     : hello}
# parse p2 "";;
Exception: Cohttp_eio__Parser.P.Parse_failure "parse error".
```
## String/Char: char, satisfy, string, peek_string, peek_char, *>, <*

```ocaml
let p1 = P.(string "GET" *> char ' ' *> peek_string 10)
let p2 = P.peek_char
let p3 = P.(satisfy (function 'A' | 'B' -> true | _ -> false))
let p4 = P.(string "GET" *> char ' ' *> char '/' *> char ' ')
```

```ocaml
# parse p1 "GET / HTTP/1.1";;
- : string t = {reader_len : 10;
                pos        : 4;
                result     : / HTTP/1.1}
# parse p2 "GET";;
- : char t = {reader_len = 3; pos = 0; result = 'G'; reader = <abstr>}
# parse p3 "ABA";;
- : char t = {reader_len = 2; pos = 1; result = 'A'; reader = <abstr>}
# parse p4 "GET / ";;
- : char t = {reader_len = 0; pos = 6; result = ' '; reader = <abstr>}
```

## Take: take_while, take_while1, take_bigstring, take

```ocaml
let f = function 'A' | 'B' | 'C' -> true | _ -> false
let p1 = P.take_while f 
let p2 = P.take_while1 f
let p3 = P.take_bigstring 4
let p4 = P.take 4
```
```ocaml
# parse p1 "ABCD";;
- : string t = {reader_len : 1;
                pos        : 3;
                result     : ABC}
# parse p1 "DDD";;
- : string t = {reader_len : 3;
                pos        : 0;
                result     : }
# parse p2 "ABCD";;
- : string t = {reader_len : 1;
                pos        : 3;
                result     : ABC}
# parse p2 "DDD";;
Exception:
Cohttp_eio__Parser.P.Parse_failure "[take_while1] count is less than 1".
# parse p3 "DDDD";;
- : P.bigstring t =
{reader_len = 0; pos = 4; result = DDDD; reader = <abstr>}
# parse p3 "DDD";;
Exception:
Cohttp_eio__Parser.P.Parse_failure "[take_bigstring] not enough input".
# parse p4 "DDDD";;
- : string t = {reader_len : 0;
                pos        : 4;
                result     : DDDD}
# parse p4 "DDD";;
Exception: Cohttp_eio__Parser.P.Parse_failure "[take] not enough input".
```

## Skip: skip, skip_while, skip_many 

```ocaml
let f = function 'A' | 'B' | 'C' -> true | _ -> false
let p1 = P.skip f
let p2 = P.skip_while f
let p3 = P.(skip_many (satisfy f))
```
```ocaml
# parse p1 "ABCD";;
- : unit t = {reader_len = 3; pos = 1; result = (); reader = <abstr>}
# parse p2 "ABCD";;
- : unit t = {reader_len = 1; pos = 3; result = (); reader = <abstr>}
# parse p3 "ABCD";;
- : unit t = {reader_len = 1; pos = 3; result = (); reader = <abstr>}
```

## Reuse reader in-between parsing

```ocaml
let p4 = P.(char 'D' *> char ' ' *> string "hello world")
```
```ocaml
# let r = parse p2 "ABCD hello world!";;
val r : unit t = {reader_len = 14; pos = 3; result = (); reader = <abstr>}
# parse ~rdr:r.reader p4 "";;
- : string t = {reader_len : 1;
                pos        : 13;
                result     : hello world}
```

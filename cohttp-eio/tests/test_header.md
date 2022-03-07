```ocaml
open Cohttp_eio
```
```ocaml
# #install_printer Header.pp;;
```
## Create and Add HTTP headers

`Header.t` is created via `create, empty`.

```ocaml
# let t = Header.create 1;;
val t : Header.t = Header [ length=0 { }]

# let a = Header.empty;;
val a : Header.t = Header [ length=0 { }]
```

Use `add`, `add_header`, `add_multi` and `add_list` to add headers. Duplicates - headers with the same key/name - are allowed.

```ocaml
# Header.add_header t ("nm1", "val1");;
- : unit = ()

# Header.add_header t ("nm2", "val2");;
- : unit = ()

# Header.add t "nm3" "val3";;
- : Header.t = Header [ length=3 {nm1 = "val1"; nm2 = "val2"; nm3 = "val3" }]

# Header.add t "nm4" "val4";;
- : Header.t = Header [ length=4
{nm1 = "val1"; nm2 = "val2"; nm3 = "val3"; nm4 = "val4" }]

# Header.add t "nm4" "val4_2";;
- : Header.t = Header [ length=5
{nm1 = "val1"; nm2 = "val2"; nm3 = "val3"; nm4 = "val4"; nm4 = "val4_2" }]

# Header.add_multi t "mult_key" ["mult_v1"; "mult_v2"];;
- : Header.t = Header [ length=7
{nm1 = "val1"; nm2 = "val2"; nm3 = "val3"; nm4 = "val4"; nm4 = "val4_2";
 mult_key = "mult_v1"; mult_key = "mult_v2"
}]

# Header.add_list t [("list_nm1", "list_v1"); ("list_nm2", "list_v2")];;
- : Header.t = Header [ length=9
{nm1 = "val1"; nm2 = "val2"; nm3 = "val3"; nm4 = "val4"; nm4 = "val4_2";
 mult_key = "mult_v1"; mult_key = "mult_v2"; list_nm1 = "list_v1";
 list_nm2 = "list_v2"
}]
```

`add_unless_exists` adds header iff the key doesn't already exist. Below "nm1" already exists so it isn't added again.

```ocaml
# Header.add_unless_exists t "nm1" "val";;
- : Header.t = Header [ length=9
{nm1 = "val1"; nm2 = "val2"; nm3 = "val3"; nm4 = "val4"; nm4 = "val4_2";
 mult_key = "mult_v1"; mult_key = "mult_v2"; list_nm1 = "list_v1";
 list_nm2 = "list_v2"
}]
```

"new1" doesn't exist yet so it is added to `t`.

```ocaml
# Header.add_unless_exists t "new1" "new_val1";;
- : Header.t = Header [ length=10
{nm1 = "val1"; nm2 = "val2"; nm3 = "val3"; nm4 = "val4"; nm4 = "val4_2";
 mult_key = "mult_v1"; mult_key = "mult_v2"; list_nm1 = "list_v1";
 list_nm2 = "list_v2"; new1 = "new_val1"
}]
```

## Query Header.t  
 
```ocaml
# Header.is_empty t;;
- : bool = false

# Header.(is_empty empty);;
- : bool = true

# Header.length t;;
- : int = 10

# Header.compare t t;;
- : int = 0

# Header.(compare t empty);;
- : int = 1

# Header.mem t "nm1";;
- : bool = true

# Header.mem t "non_existing_header";;
- : bool = false

# Header.find t "nm1";;
- : string = "val1"

# Header.find t "non_existing_key";;
Exception: Not_found.

# Header.find_opt t "nm1";;
- : string option = Some "val1"

# Header.find_opt t "non_existing_key";;
- : string option = None

# Header.find_multi t "mult_key";;
- : string list = ["mult_v2"; "mult_v1"]

# Header.find_multi t "non_existing_key";;
- : string list = []
```

## to_string

```ocaml
# Header.to_string t;;
- : string =
"nm1: val1\r\nnm2: val2\r\nnm3: val3\r\nnm4: val4\r\nnm4: val4_2\r\nmult_key: mult_v1\r\nmult_key: mult_v2\r\nlist_nm1: list_v1\r\nlist_nm2: list_v2\r\nnew1: new_val1\r\n\r\n"
```
## Remove header

`remove` removes all occurences of header matching the key.

```ocaml
# Header.remove t "nm1";;
- : Header.t = Header [ length=9
{nm2 = "val2"; nm3 = "val3"; nm4 = "val4"; nm4 = "val4_2";
 mult_key = "mult_v1"; mult_key = "mult_v2"; list_nm1 = "list_v1";
 list_nm2 = "list_v2"; new1 = "new_val1"
}]

# Header.remove t "new1";;
- : Header.t = Header [ length=8
{nm2 = "val2"; nm3 = "val3"; nm4 = "val4"; nm4 = "val4_2";
 mult_key = "mult_v1"; mult_key = "mult_v2"; list_nm1 = "list_v1";
 list_nm2 = "list_v2"
}]
```

header with key "nm4" has 2 entries. Both of them will be removed by `remove`.
```ocaml
# Header.remove t "nm4";;
- : Header.t = Header [ length=6
{nm2 = "val2"; nm3 = "val3"; mult_key = "mult_v1"; mult_key = "mult_v2";
 list_nm1 = "list_v1"; list_nm2 = "list_v2"
}]

# Header.remove t "non_existent";;
Exception: Not_found.
```

## replace

```ocaml
# Header.replace t "nm2" "replaced_value";;
- : Header.t = Header [ length=6
{nm2 = "replaced_value"; nm3 = "val3"; mult_key = "mult_v1";
 mult_key = "mult_v2"; list_nm1 = "list_v1"; list_nm2 = "list_v2"
}]

# Header.replace t "new2" "new2_value";;
- : Header.t = Header [ length=7
{nm2 = "replaced_value"; nm3 = "val3"; mult_key = "mult_v1";
 mult_key = "mult_v2"; list_nm1 = "list_v1"; list_nm2 = "list_v2";
 new2 = "new2_value"
}]
```

## Clear Header.t

`clears` resets the length to 0.

```ocaml
# Header.clear t;;
- : unit = ()

# Header.length t;;
- : int = 0

# Header.add_header t ("nm1", "val1");;
- : unit = ()

# Header.length t;;
- : int = 1
```

## is_keep_alive

```ocaml
# Header.is_keep_alive t;;
- : bool = false
```

```ocaml
Header.add t "Connection" "keep-alive";;
```

```ocaml
# Header.is_keep_alive t;;
- : bool = true
```

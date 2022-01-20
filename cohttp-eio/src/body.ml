type 'a t = [ `String of Cstruct.t | `Chunked of 'a read_chunk ]
and 'a read_chunk = (chunk -> unit) -> [ `Ok of 'a | `Eof ]

and chunk =
  | Chunk of {
      data : Cstruct.t;
      length : int;
      extensions : chunk_extension list;
    }
  | Last_chunk of chunk_extension list

and chunk_extension = { name : string; value : string option }

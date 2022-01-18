type t = [ `String of Cstruct.t | `Chunked of read_chunk ]
and read_chunk = Http.Request.t -> (chunk -> unit) -> unit

and chunk =
  | Chunk of {
      data : Cstruct.t;
      length : int;
      extensions : chunk_extension list;
    }
  | Last_chunk of {
      extensions : chunk_extension list;
      updated_request : Http.Request.t;
    }

and chunk_extension = { name : string; value : string option }

type t =
  | Chunk of {
      data : Cstruct.t;
      length : int;
      extensions : chunk_extension list;
    }
  | Last_chunk of chunk_extension list

and chunk_extension = { name : string; value : string option }

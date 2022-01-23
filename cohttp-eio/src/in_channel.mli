type t

val create : ?bufsize:int -> #Eio.Flow.read -> t
val default_io_buffer_size : int
val read_into : t -> off:int -> len:int -> Bigstringaf.t -> int

val read_char : t -> char
(** [read_char t] reads and returns [char] from [t] or raises [End_of_file] if
    [t] has reached end of input.*)

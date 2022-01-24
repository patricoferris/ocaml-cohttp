(** [Reader] represents a buffered reader *)

type t

val create : ?buffer_size:int -> #Eio.Flow.read -> t
(** [create ?buffer_size reader] creates [t]. [buffer_size] is the maximum
    number of bytes [reader] attempts to read in one call. If [buffer_size] is
    not given then [default_io_buffer_size] is used. *)

val reader : t -> Eio.Flow.read
(** [reader t] returns the reader used by [t]. *)

val buffer_size : t -> int
(** [bufer_size t] returns the current [reader t] read buffer size. *)

val default_io_buffer_size : int
(** [default_io_buffer_size] is [4096]. *)

(** {1 Low Level API} *)

type offset = int
type length = int

val consume : t -> int -> unit
(** [consume t n] marks [n] bytes of data as consumed in [t]. *)

val feed_input : t -> Bigstringaf.t * offset * length
(** [feed_input t] is [(buf, off, len)]. Attempts to read at most
    [buffer_size t] bytes into [t] and returns a view into unconsumed buffer
    represented by [(buf, off, len)]. [len] is 0 if [reader t] has reached end
    of file. *)

(** {2 High Level API} *)

val read_into : t -> off:int -> len:int -> Bigstringaf.t -> int
(** [read_into t ~off ~len buf] fills [buf] from [off] to length [len] with data
    from [t]. *)

val read_char : t -> char
(** [read_char t] reads and returns [char] from [t] or raises [End_of_file] if
    [t] has reached end of input.*)

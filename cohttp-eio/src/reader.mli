type t

val create : int -> (Cstruct.t -> int) -> t
(** [create len read_fn] returns [t] with an initial buffer of size [len].

    [read_fn] is the read function used to fill [t]. It should return [0] to
    indicate end of file and [n] to indicate the count of bytes read. *)

val buffer : t -> Bigstringaf.t
(** [buffer t] is the unconsumed bytes in [t]. *)

val length : t -> int
(** [length t] is the count of unconsumed bytes in [t]. *)

val consume : t -> int -> unit
(** [consume t n] marks [n] bytes of data as consumed in [t]. *)

val fill : t -> int -> int
(** [fill t n] attempts to fill [t] with [n] bytes. It returns [0] if end of
    file is reached. Otherwise it return the number of bytes stored in [t]. *)

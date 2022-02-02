(** [Reader] is a buffered reader. *)
module Reader : sig
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

  val consume : t -> int -> unit
  (** [consume t n] marks [n] bytes of data as consumed in [t]. *)

  val feed_input : t -> Cstruct.t
  (** [feed_input t] is [buf]. Attempts to read at most [buffer_size t] bytes
      into [t] and returns a view into unconsumed buffer represented by [buf].
      [buf.len = 0] if [reader t] has reached end of file. *)

  (** {2 High Level API} *)

  val read_into : t -> off:int -> len:int -> Cstruct.buffer -> int
  (** [read_into t ~off ~len buf] fills [buf] from [off] to length [len] with
      data from [t]. *)

  val read_char : t -> char
  (** [read_char t] reads and returns [char] from [t] or raises [End_of_file] if
      [t] has reached end of input.*)

  exception Parse_error of string

  val parse : t -> 'a Angstrom.t -> 'a
  (** [parse t p] is [a] after an Angstrom parser [p] is successfully executed
      over [t].

      @raise Parse_error if an error is encountered during parsing. *)
end

(** [Chunk] encapsulates HTTP/1.1 chunk transfer encoding data structures.
    https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
module Chunk : sig
  type t =
    | Chunk of {
        data : Cstruct.t;
        length : int;
        extensions : chunk_extension list;
      }
    | Last_chunk of chunk_extension list

  and chunk_extension = { name : string; value : string option }
end

(** [Request] is a HTTP/1.1 request. *)
module Request : sig
  type t

  (** {1 Request Details} *)

  val has_body : t -> [ `No | `Unknown | `Yes ]
  val headers : t -> Http.Header.t
  val meth : t -> Http.Method.t
  val scheme : t -> string option
  val resource : t -> string
  val version : t -> Http.Version.t
  val is_keep_alive : t -> bool

  (** {1 Builtin Request Body Readers} *)

  val read_fixed : t -> (Cstruct.t, string) result
  (** [read_fixed t] is [Ok buf] if "Content-Length" header value exists in [t]
      and is a valid integer value. Otherwise it is [Error err] where [err] is
      the error text. *)

  val read_chunk : t -> (Chunk.t -> unit) -> (t, string) result
  (** [read_chunk t f] is [Ok req] if "Transfer-Encoding" header value exists,
      is "chunked" in [t] and all chunks in a request are read successfully.
      [req] is the updated request as specified by the chunked encoding
      algorithm in https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3.
      Otherwise it is [Error err] where [err] is the error text. *)

  (** {1 Custom Request Body Readers} *)

  val reader : t -> Reader.t
  (** [reader t] returns a [Reader.t] instance. This can be used to create a
      custom request body reader. *)

  val set_read_complete : t -> unit
  (** [set_read_complet t] indicates that request [t] body has been read. *)
end

(** [Response] is a HTTP/1.1 response. *)
module Response : sig
  type t

  and body =
    [ `String of Cstruct.t
    | `Chunked of write_chunk
    | `Custom of Faraday.t -> unit
    | `None ]

  and write_chunk = (Chunk.t -> unit) -> unit

  val create : ?headers:Http.Header.t -> ?status:Http.Status.t -> body -> t

  (** {1 Response Details} *)

  val headers : t -> Http.Header.t
  val status : t -> Http.Status.t
  val body : t -> body

  (** {1 Basic Response} *)

  val text : string -> t
  (** [text s] is a HTTP/1.1, 200 status response with header "Content-Type:
      text/plain". *)

  val html : string -> t
  (** [html s] is a HTTP/1.1, 200 status response with header "Content-Type:
      text/html". *)

  val not_found : t
  (** [not_found] is a HTTP/1.1, 404 status response. *)

  val internal_server_error : t
  (** [internal_server_error] is a HTTP/1.1, 500 status response. *)

  val bad_request : t
  (** [bad_request] is a HTTP/1.1, 400 status response. *)
end

(** [Server] is a HTTP 1.1 server. *)
module Server : sig
  type t
  type handler = Request.t -> Response.t option
  type middleware = handler -> handler

  (** {1 Run Server} *)

  val create : ?socket_backlog:int -> ?domains:int -> port:int -> handler -> t
  val run : t -> unit
  val close : t -> unit

  (** {1 Basic Handlers} *)

  val not_found : handler

  (** {1 Handler Combinators} *)

  val join : handler -> handler -> handler
  (** [join h1 h2] executes handler [h1]. If response is [None] then it executes
      handler [h2]. *)

  module Infix : sig
    val ( >>? ) : handler -> handler -> handler
    (** [h1 >>? h2] is [join h1 h2] *)
  end
end

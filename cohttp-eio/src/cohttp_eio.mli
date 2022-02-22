(** [Reader] is a buffered reader. *)
module Reader : sig
  type t

  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val create : ?buffer_size:int -> Eio.Flow.source -> t
  (** [create ?buffer_size reader] creates [t]. [buffer_size] is the maximum
      number of bytes [reader] attempts to read in one call. If [buffer_size] is
      not given then [default_io_buffer_size] is used. *)

  val default_io_buffer_size : int
  (** [default_io_buffer_size] is [4096]. *)

  (** {1 Low Level API} *)

  val source : t -> Eio.Flow.source
  (** [source t] returns the reader used by [t]. *)

  (* val buffer : t -> bigstring *)
  (** [buffer t] is the unconsumed bytes in [t]. *)

  val length : t -> int
  (** [length t] is the count of unconsumed bytes in [t]. *)

  val buffer_size : t -> int
  (** [bufer_size t] returns the read buffer size of [t] *)

  val consume : t -> int -> unit
  (** [consume t n] marks [n] bytes of data as consumed in [t]. *)

  val fill : t -> int -> unit
  (** [fill t sz] attempts to read at least [sz] count of bytes into [t].

      @raise End_of_file if [source t] has reached end of file. *)

  (*exception Parse_error of string *)

  (*val parse : t -> 'a Angstrom.t -> 'a *)
  (*(1** [parse t p] is [a] after an Angstrom parser [p] is successfully executed *)
  (*    over [t]. *)

  (*    @raise Parse_error if an error is encountered during parsing. *)
  (*    @raise End_of_file if [t] has reached end of file. *1) *)
end

(** [Chunk] encapsulates HTTP/1.1 chunk transfer encoding data structures.
    https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
module Chunk : sig
  type t =
    | Chunk of { size : int; data : Cstruct.t; extensions : extension list }
    | Last_chunk of extension list

  and extension = { name : string; value : string option }
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
  (** [read_fixed t] is [Ok buf] if "Content-Length" header is a valid integer
      value in [t]. Otherwise it is [Error err] where [err] is the error text. *)

  val read_chunk : t -> (Chunk.t -> unit) -> (t, string) result
  (** [read_chunk t f] is [Ok req] if "Transfer-Encoding" header value is
      "chunked" in [t] and all chunks in a request are read successfully. [req]
      is the updated request as specified by the chunked encoding algorithm in
      https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3. Otherwise it
      is [Error err] where [err] is the error text. *)

  (** {1 Custom Request Body Readers} *)

  val reader : t -> Eio.Buf_read.t
  (** [reader t] returns a [Reader.t] instance. This can be used to create a
      custom request body reader. *)

  val set_read_complete : t -> unit
  (** [set_read_complet t] indicates that request [t] body has been read. *)
end

(** [Response] is a HTTP/1.1 response. *)
module Response : sig
  type t

  and body =
    [ `String of string
    | `Chunked of write_chunk
    | `Custom of Eio.Flow.sink -> unit
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
  type handler = Request.t -> Response.t
  type middleware = handler -> handler

  (** {1 Run Server} *)

  val create : ?socket_backlog:int -> ?domains:int -> port:int -> handler -> t
  val run : t -> Eio.Stdenv.t -> unit
  val stop : t -> unit

  (** {1 Basic Handlers} *)

  val not_found : handler

  (** {1 Handler Combinators} *)

  (* val join : handler -> handler -> handler *)
  (** [join h1 h2] executes handler [h1]. If response is [None] then it executes
      handler [h2]. *)

  (*module Infix : sig *)
  (*  val ( >>? ) : handler -> handler -> handler *)
  (*  (1** [h1 >>? h2] is [join h1 h2] *1) *)
  (*end *)
end

(**/**)

module Private : sig
  module Parser : sig
    type 'a t

    type bigstring =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

    exception Parse_failure of string

    val return : 'a -> 'a t
    val fail : string -> 'a t
    val ( <?> ) : 'a t -> string -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <* ) : 'a t -> _ t -> 'a t
    val ( *> ) : _ t -> 'b t -> 'b t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val lift : ('a -> 'b) -> 'a t -> 'b t
    val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val end_of_input : unit t
    val pos : int t
    val option : 'a -> 'a t -> 'a t
    val peek_char : char t
    val peek_string : int -> string t
    val char : char -> char t
    val satisfy : (char -> bool) -> char t
    val string : string -> string t
    val take_while1 : (char -> bool) -> string t
    val take_while : (char -> bool) -> string t
    val take_bigstring : int -> bigstring t
    val take : int -> string t
    val many : 'a t -> 'a list t
    val many_till : 'a t -> _ t -> 'a list t
    val skip : (char -> bool) -> unit t
    val skip_while : (char -> bool) -> unit t
    val skip_many : 'a t -> unit t
    val parse : Eio.Buf_read.t -> 'a t -> 'a
  end
end

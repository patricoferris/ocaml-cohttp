(** [Version] is a HTTP version *)
module Version : sig
  type t = HTTP_1_1 | HTTP_1_0

  val to_string : t -> string

  val of_string : string -> t
  (** [of_string s] converts string [s] to [t].

      [HTTP/1.0] is converted to [HTTP_1_0] [http/1.1] IS converted to
      [HTTP_1_1]

      @raise if [s] is neither of above. *)

  val compare : t -> t -> int
end

(** [Method] is a HTTP method. *)
module Method : sig
  type t =
    | GET
    | POST
    | HEAD
    | DELETE
    | PATCH
    | PUT
    | OPTIONS
    | TRACE
    | CONNECT
    | Other of string

  val to_string : t -> string
  val of_string : string -> t
  val compare : t -> t -> int
end

(** [Header] represents a collection of HTTP header values in Request and
    Response. *)
module Header : sig
  type t

  val create : int -> t
  (** [create n] creates [t] with initial buffer size of [n] *)

  val init : unit -> t
  (** [init ()] is an empty header. *)

  (** {1 Add HTTP header} *)

  val add_header : t -> string * string -> unit
  val add : t -> string -> string -> unit
  val add_unless_exists : t -> string -> string -> unit
  val add_list : t -> (string * string) list -> unit
  val add_multi : t -> string -> string list -> unit

  (** {1 Find headers} *)

  val find : t -> string -> string
  (** [find t key] returns value [v] associated with header key [key].

      @raise Not_found if header with [key] is not found in [t]. *)

  val find_opt : t -> string -> string option
  (** [find_opt t key] returns [Some v] for header with [key] in [t]. None if
      [key] is not found in [t] *)

  val find_multi : t -> string -> string list
  (** [find_multi t key] returns all values associated with header with [key]. *)

  val mem : t -> string -> bool
  (**[mem t key] returns [true] if [key] is found in [t]. *)

  (** {1 Remove Headers} *)

  val remove : t -> string -> unit
  (** [remove t key] removes header with [key] in [t].

      @raise Not_found if [key] doesn't exist in [t]. *)

  (** {1 Update Header} *)

  val replace : t -> string -> string -> unit
  (** [replace t key value] replaces the value of the first HTTP header
      associated with [key]. *)

  val update : t -> string -> (string option -> string option) -> unit

  (** {1 Iterators} *)

  val iter : (string -> string -> unit) -> t -> unit
  val map : (string -> string -> string) -> t -> t
  val fold_left : (string -> string -> 'a -> 'a) -> 'a -> t -> 'a

  (** {1 Header properties} *)

  val is_empty : t -> bool

  val length : t -> int
  (** [length t] returns the count of headers in [t]. *)

  (** {1 Compare} *)

  val compare : t -> t -> int

  (** {1 Clear} *)

  val clear : t -> unit
  (** [clear t] clears the current headers in [t]. *)

  (** {1 Conversions/} *)

  val to_string : t -> string

  (**{1 Printers} *)

  val pp : Format.formatter -> t -> unit
end

(** [Reader] is a buffered reader. *)
module Reader : sig
  type t

  val length : t -> int
  (** [length t] is the count of unconsumed bytes in [t]. *)

  val consume : t -> int -> unit
  (** [consume t n] marks [n] bytes of data as consumed in [t]. *)

  val fill : t -> int -> int
  (** [fill t n] attempts to fill [t] with [n] bytes and returns the actual
      number of bytes filled.

      @raise End_of_file if end of file is reached. *)

  val unsafe_get : t -> int -> char
  val substring : t -> off:int -> len:int -> string
  val copy : t -> off:int -> len:int -> Bigstringaf.t
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

  val headers : t -> Header.t
  val meth : t -> Method.t
  val resource : t -> string
  val version : t -> Version.t
  val is_keep_alive : t -> bool

  (** {1 Builtin Request Body Readers} *)

  val read_fixed : t -> (string, string) result
  (** [read_fixed t] is [Ok buf] if "Content-Length" header is a valid integer
      value in [t]. Otherwise it is [Error err] where [err] is the error text. *)

  (* val read_chunk : t -> (Chunk.t -> unit) -> (t, string) result *)
  (** [read_chunk t f] is [Ok req] if "Transfer-Encoding" header value is
      "chunked" in [t] and all chunks in a request are read successfully. [req]
      is the updated request as specified by the chunked encoding algorithm in
      https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3. Otherwise it
      is [Error err] where [err] is the error text. *)

  (** {1 Custom Request Body Readers} *)

  val reader : t -> Reader.t
  (** [reader t] returns a [Reader.t] instance. This can be used to create a
      custom request body reader. *)
end

(** [Response] is a HTTP/1.1 response. *)
module Response : sig
  type t

  and body =
    | String of string
    | Chunked of write_chunk
    | Custom of (Eio.Flow.sink -> unit)
    | Empty

  and write_chunk = (Chunk.t -> unit) -> unit

  (** {1 Response Details} *)

  val headers : t -> Header.t
  val status : t -> Http.Status.t
  val body : t -> body

  (** {1 Configuring Basic Response} *)

  val text : string -> t
  (** [text t s] returns a HTTP/1.1, 200 status response with "Content-Type"
      header set to "text/plain". *)

  val html : string -> t
  (** [html t s] returns a HTTP/1.1, 200 status response with header set to
      "Content-Type: text/html". *)

  val not_found : t
  (** [not_found t] returns a HTTP/1.1, 404 status response. *)

  val internal_server_error : t
  (** [internal_server_error] returns a HTTP/1.1, 500 status response. *)

  val bad_request : t
  (** [bad_request t] returns a HTTP/1.1, 400 status response. *)
end

(** [Server] is a HTTP 1.1 server. *)
module Server : sig
  type t
  type handler = Request.t -> Response.t
  type middleware = handler -> handler

  (** {1 Run Server} *)

  val create : ?socket_backlog:int -> ?domains:int -> port:int -> handler -> t
  val run : t -> Eio.Stdenv.t -> unit

  (** {1 Basic Handlers} *)

  val not_found : handler
end

(**/**)

module Private : sig
  module Parser : sig
    type 'a t = Reader.t -> 'a

    exception Parse_failure of string

    val return : 'a -> 'a t
    val fail : string -> 'a t
    val commit : unit t
    val pos : int t
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
    val end_of_input : bool t
    val option : 'a -> 'a t -> 'a t
    val peek_char : char t
    val peek_string : int -> string t
    val char : char -> unit t
    val any_char : char t
    val satisfy : (char -> bool) -> char t
    val string : string -> unit t
    val take_while1 : (char -> bool) -> string t
    val take_while : (char -> bool) -> string t
    val take_bigstring : int -> Bigstringaf.t t
    val take : int -> string t
    val take_till : (char -> bool) -> string t
    val many : 'a t -> 'a list t
    val skip : (char -> bool) -> unit t
    val skip_while : (char -> bool) -> unit t
    val skip_many : 'a t -> unit t
  end
end

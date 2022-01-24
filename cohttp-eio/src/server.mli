type t
(** [t] represents a HTTP 1.1 server. *)

(** [Request_body_reader] provides functions to read HTTP request body. *)
module type Request_body_reader = sig
  (** {1 Builtin Request Body Readers} *)

  val read_fixed : unit -> (Cstruct.t, string) result
  val read_chunk : (Body.chunk -> unit) -> (Http.Request.t, string) result

  (** {1 Custom Request Body Readers} *)

  val reader : Reader.t
  (** [reader] returns a [Reader.t] instance. This can be used to create a
      custom request body reader. *)

  val set_read_complete : unit -> unit
end

type request = Http.Request.t * (module Request_body_reader)

type response = (Http.Response.t * response_body) option
and response_body = [ unit Body.t | `Custom of Faraday.t -> unit ]

type handler = request -> response
type middleware = handler -> handler

(** {1 Run Server} *)

val create : ?socket_backlog:int -> ?domains:int -> port:int -> handler -> t
val run : t -> unit
val close : t -> unit

(** {1 Basic Response} *)

val text : string -> response
val html : string -> response

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

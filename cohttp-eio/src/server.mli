type t

module Client_connection : sig
  type t

  val client_addr : t -> Eio.Net.Sockaddr.t
  val switch : t -> Eio.Std.Switch.t
  val close : t -> unit
  val ic : t -> Eio.Flow.read
  val oc : t -> Eio.Flow.write
end

type response =
  [ `Response of Http.Response.t * Cstruct.t
  | `Expert of Client_connection.t -> unit ]

type request_handler = Request.t * Request.body -> response

val create :
  ?socket_backlog:int -> ?domains:int -> port:int -> request_handler -> t

val run : t -> unit
val close : t -> unit

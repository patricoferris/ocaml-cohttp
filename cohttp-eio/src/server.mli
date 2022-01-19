module Client_connection : sig
  type t

  val client_addr : t -> Eio.Net.Sockaddr.t
  val switch : t -> Eio.Std.Switch.t
  val close : t -> unit
  val ic : t -> Eio.Flow.read
  val oc : t -> Eio.Flow.write
end

type t
type request = Request.t * Request.body option
type response = Http.Response.t * Cstruct.t option
type handler = request -> response
type middleware = handler -> handler

val create : ?socket_backlog:int -> ?domains:int -> port:int -> handler -> t
val run : t -> unit
val close : t -> unit

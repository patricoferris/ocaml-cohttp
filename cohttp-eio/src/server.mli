type t
type request = Http.Request.t * Http.Request.t Body.t option

type response = Http.Response.t * response_body
and response_body = [ unit Body.t | `Custom of Faraday.t -> unit ] option

type handler = request -> response
type middleware = handler -> handler

val create : ?socket_backlog:int -> ?domains:int -> port:int -> handler -> t
val run : t -> unit
val close : t -> unit

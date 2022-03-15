(* Original https://github.com/talex5/httpaf/blob/eio/lib/serialize.ml *)
module Optional_thunk : sig
  type t

  val none : t
  val some : (unit -> unit) -> t
  val call_if_some : t -> unit
end = struct
  type t = unit -> unit

  let none = Sys.opaque_identity (fun () -> ())

  let some f =
    if f == none then
      failwith
        "Optional_thunk: this function is not representable as a some value";
    f

  let call_if_some t = t ()
end

type t = {
  flow : < Eio.Flow.two_way ; Eio.Flow.close >;
  data : string Queue.t;
  mutable wakeup : Optional_thunk.t;
}

let create flow = { flow; data = Queue.create (); wakeup = Optional_thunk.none }
let write_string t s = Queue.push s t.data
let sink t = (t.flow :> Eio.Flow.sink)
let close t = Eio.Flow.close t.flow

let wakeup t =
  let f = t.wakeup in
  t.wakeup <- Optional_thunk.none;
  Optional_thunk.call_if_some f

let run t =
  let rec loop () =
    match Queue.take_opt t.data with
    | None -> t.wakeup <- Optional_thunk.some loop
    | Some s ->
        Eio.Flow.copy_string s t.flow;
        loop ()
  in
  loop ()

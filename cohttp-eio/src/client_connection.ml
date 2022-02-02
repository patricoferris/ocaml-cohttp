type t = {
  flow : < Eio.Flow.two_way ; Eio.Flow.close >;
  switch : Eio.Std.Switch.t;
  addr : Eio.Net.Sockaddr.t;
  reader : Reader.t;
  oc : Eio.Flow.write;
}

let close t = Eio.Flow.close t.flow

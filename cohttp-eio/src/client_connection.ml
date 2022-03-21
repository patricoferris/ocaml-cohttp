type t = {
  flow : < Eio.Flow.two_way ; Eio.Flow.close >;
  switch : Eio.Std.Switch.t;
  addr : Eio.Net.Sockaddr.stream;
  reader : Reader.t;
  response_buffer : Buffer.t;
}

let close t = Eio.Flow.close t.flow
let clear t = Buffer.clear t.response_buffer

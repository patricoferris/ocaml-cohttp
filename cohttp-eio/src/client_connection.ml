type t = {
  flow : < Eio.Flow.two_way ; Eio.Flow.close >;
  switch : Eio.Std.Switch.t;
  addr : Eio.Net.Sockaddr.t;
  reader : Eio.Buf_read.t;
  response_buf : Buffer.t;
}

let close t = Eio.Flow.close t.flow

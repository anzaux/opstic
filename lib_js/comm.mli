open Types

type 'a ep = 'a Witness.ep
type 'a inp = 'a Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out
type 'x service_spec = 'x Witness.service_spec
type 'x service

val send : 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep io
val receive : ([> ] as 'b) inp ep -> 'b io
val close : unit ep -> unit

val start_service :
  ServerImpl.server -> 'a inp Witness.service_spec -> ('a -> unit io) -> unit

val register_service : ServerImpl.server -> 'x service_spec -> 'x service
val accept : ServerImpl.server -> 'a inp service -> 'a io

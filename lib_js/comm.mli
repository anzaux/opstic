open Types

type 'a ep
type 'a inp = 'a Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out
type 'x service

val send : 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep io
val receive : ([> ] as 'b) inp ep -> 'b io
val close : unit ep -> unit

val start_service :
  Server.t -> 'a inp Witness.service_spec -> ('a -> unit io) -> 'b io

val register_service : Server.t -> 'x Witness.service_spec -> 'x service
val accept : Server.t -> 'a inp service -> 'a io

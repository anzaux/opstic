type 'a ep
type 'a inp = 'a Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out
type 'x service_spec = 'x Witness.service_spec

val send : 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep Monad.t
val receive : ([> ] as 'b) inp ep -> 'b Monad.t
val close : unit ep -> unit

val start_service :
  Server.t -> 'x service_spec -> ('x -> unit Monad.t) -> unit Monad.t

type 'a ep = 'a Witness.ep = {
  ep_raw : ServerEndpoint.t;
  ep_witness : 'a Lin.t;
}

type 'a inp = 'a Witness.inp constraint 'a = [> ]
type ('v, 'a) out = ('v, 'a) Witness.out

val send : 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep ServerIo.t
val receive : ([> ] as 'b) inp ep -> 'b ServerIo.t
val close : unit ep -> unit
val create : ServerEndpoint.t -> 'a -> 'a ep

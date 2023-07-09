type 'a ep

type 'a inp = 'a Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out

val send : 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep ServerIo.t
val receive : ([> ] as 'b) inp ep -> 'b ServerIo.t
val close : unit ep -> unit
(* val create : ServerEt -> 'a -> 'a ep *)

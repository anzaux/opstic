(* type 'a service
   type 'a ep = 'a Comm.ep
   type 'a inp = 'a Comm.inp
   type ('v, 'a) out = ('v, 'a) Comm.out

   val init : 'a service -> 'a ep ServerIo.t
   val init_send : 'a service -> ('a -> ('v, 'b) out) -> 'v -> 'b ep ServerIo.t
   val init_receive : ([> ] as 'b) inp service -> 'b ServerIo.t *)

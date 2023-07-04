(* type 'a service = { sv_raw : Service.t; sv_witness : 'a }
type 'a ep = 'a ep
type 'a inp = 'a Comm.inp
type ('v, 'a) out = ('v, 'a) Comm.out

module EP = Make

let return = ServerIo.return
let ( let* ) = ServerIo.bind

let init sv =
  let* ep_raw = Service.init sv.sv_raw in
  return (EP.create ep_raw sv.sv_witness)

let init_send sv f v =
  let* ep = init sv in
  Comm.send ep f v

let init_receive sv =
  let* ep = init sv in
  Comm.receive ep *)

open Types
open Witness
open Server
open Monad

let ( let* ) = Monad.bind

type 'a ep = 'a Witness.ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }
type 'm inp = 'm Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out
type 'x service_spec = 'x Witness.service_spec

let receive : 'a inp ep -> 'a Monad.t =
 fun ep ->
  let inproles = Lin.get ep.ep_witness in
  let get_queue (InpRole inprole) =
    let role = Role.create inprole.role_constr.constr_name in
    match inprole.path_kind with
    | `Established ->
        let* queue = Server.Util.get_queue_ ep.ep_raw role in
        return queue.request_queue
    | _ -> return @@ Hashtbl.find ep.ep_raw.service_ref.greetings role
  in
  let* queues = List.map snd inproles |> mapM get_queue in
  let* request = ConcurrentQueue.dequeue_one_of queues in
  let (InpRole inprole) = List.assoc request.request_role inproles in
  let role = request.request_role in
  let label = inprole.parse_label request.request_body in
  let (InpLabel inplabel) = List.assoc label inprole.labels in
  let payload = inplabel.parse_payload request.request_body in
  let var =
    inprole.role_constr.make_var
      (inplabel.label_constr.make_var
         ( payload,
           {
             ep with
             ep_witness =
               Lin.create (Witness.witness (Lazy.force inplabel.cont));
           } ))
  in
  let* queue = Server.Util.get_queue_ ep.ep_raw role in
  ConcurrentQueue.add_waiter queue.response_queue
    request.request_response_resolv;
  return var

let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep Monad.t =
 fun ep call (*fun x -> x#a#lab*) v ->
  let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
  let* queue = Server.Util.get_queue_ ep.ep_raw out.out_role in
  let response =
    Server.{ response_role = out.out_role; response_body = out.out_marshal v }
  in
  ConcurrentQueue.enqueue queue.response_queue response;
  Monad.return
    {
      ep with
      ep_witness = Lin.create (Witness.witness (Lazy.force out.out_cont));
    }

let msg_closing (session : Server.session) =
  Monad.mpst_error
    (Format.asprintf "Session %a for service %a is closed" SessionId.pp
       session.session_id ServiceId.pp session.service_ref.spec.service_id)

let close (ep : unit ep) =
  ignore @@ Lin.get ep.ep_witness;
  Server.kill_session_ ep.ep_raw (msg_closing ep.ep_raw)

let start_service (t : Server.t) (spec : 'x Witness.service_spec) _f =
  Server.register_service t ~spec:spec.sv_spec;
  Monad.return ()

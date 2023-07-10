open Types

let ( let* ) = Monad.bind

type 'a ep = 'a Witness.ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }
type 'm inp = 'm Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out

let receive : 'a inp ep -> 'a Monad.t =
 fun ep ->
  let inproles = Lin.get ep.ep_witness in
  let roles = List.map fst inproles in
  let* queues = roles |> Monad.mapM (Server.Util.get_queue_ ep.ep_raw) in
  let queues = List.map (fun (qp : Server.queue) -> qp.request_queue) queues in
  let* (request : Server.http_request) =
    ConcurrentQueue.dequeue_one_of queues
  in
  let (InpRole inprole) = List.assoc request.request_role inproles in
  let label = inprole.parse_label request.request_body in
  let (InpLabel inplabel) = List.assoc label inprole.labels in
  let v = inplabel.parse_payload request.request_body in
  Monad.return
    (inprole.role_constr.make_var
       (inplabel.label_constr.make_var
          ( v,
            {
              ep with
              ep_witness =
                Lin.create (Witness.witness (Lazy.force inplabel.cont));
            } )))

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

(* let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit } *)

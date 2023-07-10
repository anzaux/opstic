open Types

let ( let* ) = Monad.bind

type 'a ep = 'a Witness.ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }
type 'm inp = 'm Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out

let receive : 'a inp ep -> 'a Monad.t =
 fun ep ->
  let inps = Lin.get ep.ep_witness in
  let roles = List.map fst inps |> List.map Role.create in
  let* queues = roles |> Monad.mapM (Server.Util.get_queuepair_ ep.ep_raw) in
  let queues =
    List.map (fun (qp : Server.queuepair) -> qp.request_queue) queues
  in
  let* (request : Server.http_request) =
    ConcurrentQueue.dequeue_one_of queues
  in
  let (InpRole inp) = List.assoc (Role.to_string request.request_role) inps in
  let label = inp.inp_role_parse_label request.request_body in
  let (InpLabel c) = List.assoc label inp.inp_role_labels in
  let v = c.inp_label_parse_payload request.request_body in
  Monad.return
    (inp.inp_role_constr.make_var
       (c.inp_label_constr.make_var
          ( v,
            {
              ep with
              ep_witness =
                Lin.create (Witness.witness (Lazy.force c.inp_label_cont));
            } )))

let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep Monad.t =
 fun ep call (*fun x -> x#a#lab*) v ->
  let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
  let role = Role.create out.out_role in
  let* queue = Server.Util.get_queuepair_ ep.ep_raw role in
  let response =
    Server.{ response_role = role; response_body = out.out_marshal v }
  in
  ConcurrentQueue.enqueue queue.response_queue response;
  Monad.return
    {
      ep with
      ep_witness = Lin.create (Witness.witness (Lazy.force out.out_cont));
    }

let msg_closing (session : Server.session) =
  Monad.mpst_error
    (Format.asprintf "Session %a for entrypoint %a is closed" SessionId.pp
       session.session_id ServiceId.pp
       session.entrypoint_ref.spec.service_id)

let close (ep : unit ep) =
  ignore @@ Lin.get ep.ep_witness;
  Server.kill_session_ ep.ep_raw (msg_closing ep.ep_raw)

(* let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit } *)

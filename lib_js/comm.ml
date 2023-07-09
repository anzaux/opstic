open Types

let ( let* ) = ServerIo.bind

type 'a ep = 'a Witness.ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }
type 'm inp = 'm Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out

let receive : 'a inp ep -> 'a ServerIo.t =
 fun ep ->
  let inp = Lin.get ep.ep_witness in
  let roles = inp.inp_roles |> List.map Role.create in
  let* queues = roles |> ServerIo.mapM (Server.Util.get_queuepair_ ep.ep_raw) in
  let queues =
    List.map (fun (qp : Server.queuepair) -> qp.request_queue) queues
  in
  let* (request : Server.http_request) =
    ConcurrentQueue.dequeue_one_of queues
  in
  let (InpChoice c) =
    Hashtbl.find inp.inp_choices
      (Role.to_string request.request_role, request.request_label)
  in
  let v = c.inp_choice_marshal request.request_body_raw in
  ServerIo.return
    (c.inp_choice_role.make_var
       (c.inp_choice_label.make_var
          ( v,
            {
              ep with
              ep_witness = Lin.create (Witness.witness c.inp_choice_next_wit);
            } )))

let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep ServerIo.t =
 fun ep call (*fun x -> x#a#lab*) v ->
  let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
  let role = Role.create out.out_role in
  let* queue = Server.Util.get_queuepair_ ep.ep_raw role in
  let response =
    Server.
      {
        response_role = role;
        response_label = out.out_label;
        response_body = assert false;
        response_body_raw = assert false;
      }
  in
  ConcurrentQueue.enqueue queue.response_queue response;
  ServerIo.return
    { ep with ep_witness = Lin.create (Witness.witness out.out_next_wit) }

let msg_closing (session : Server.session) =
  ServerIo.mpst_error
    (Format.asprintf "Session %a for entrypoint %a is closed" ConversationId.pp
       session.conversation_id EntrypointId.pp
       session.entrypoint_ref.spec.entrypoint_id)

let close (ep : unit ep) =
  ignore @@ Lin.get ep.ep_witness;
  Server.kill_session_ ep.ep_raw (msg_closing ep.ep_raw)

(* let create raw wit = { ep_raw = raw; ep_witness = Lin.create wit } *)

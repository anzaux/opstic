open Types
open Witness
open Server
open Monad

let ( let* ) = bind

type 'a ep = 'a Witness.ep = { ep_raw : Server.session; ep_witness : 'a Lin.t }
type 'm inp = 'm Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out
type 'x service = { sv_service_id : service_id; sv_witness : 'x }

let register_service t spec =
  Server.Server0.register_service t ~spec:spec.sv_spec;
  { sv_service_id = spec.sv_spec.service_id; sv_witness = spec.sv_witness }

let parse_request : type a. a inp -> session -> request -> a io =
 fun (inproles : _ inp) session request ->
  let role = request.request_pathspec.path_role in
  let (InpRole inprole) = List.assoc role inproles in
  let* label = inprole.parse_label request.request_body in
  let (InpLabel inplabel) = List.assoc label inprole.labels in
  let* payload = inplabel.parse_payload request.request_body in
  return
  @@ inprole.role_constr.make_var
       (inplabel.label_constr.make_var
          ( payload,
            {
              ep_raw = session;
              ep_witness =
                Lin.create (Witness.witness (Lazy.force inplabel.cont));
            } ))

let process_request session (inproles : _ inp) request =
  let queue = Session.queues session request.request_pathspec.path_role in
  let* var =
    (fun () -> parse_request inproles session request)
    |> Monad.handle_error ~handler:(fun err ->
           request.request_resolv (Error err);
           error err)
  in
  ConcurrentQueue.add_waiter queue.response_queue request.request_resolv;
  return var

let accept : type a. Server.Server0.t -> a inp service -> a io =
 fun t { sv_service_id; sv_witness = inp } ->
  let pathspecs =
    inp |> List.map (fun (_, InpRole inprole) -> inprole.path_spec)
  in
  let service = Server0.service t sv_service_id in
  let* session, request = Server.accept_at_paths service pathspecs in
  process_request session inp request

let receive : 'a inp ep -> 'a io =
 fun ep ->
  let inproles = Lin.get ep.ep_witness in
  let pathspecs =
    inproles |> List.map (fun (_, InpRole inprole) -> inprole.path_spec)
  in
  let* request = Server.receive_at_paths ep.ep_raw pathspecs in
  process_request ep.ep_raw inproles request

let send : 'a 'b. 'a ep -> ('a -> ('v, 'b) out) -> 'v -> 'b ep io =
 fun ep call v ->
  let out : ('v, 'b) out = call (Lin.get ep.ep_witness) in
  let queue = Session.queues ep.ep_raw out.out_role in
  let* response = out.out_unparse (Session.id ep.ep_raw) out.out_label v in
  ConcurrentQueue.enqueue queue.response_queue response;
  return
    {
      ep with
      ep_witness = Lin.create (Witness.witness (Lazy.force out.out_cont));
    }

let msg_closing (session : Server.session) =
  Monad.mpst_error
    (Format.asprintf "Session %a for service %a is closed" SessionId.pp
       (Session.id session) ServiceId.pp
       (Service.id (Session.service session)))

let close (ep : unit ep) =
  ignore @@ Lin.get ep.ep_witness;
  Session.kill ep.ep_raw (msg_closing ep.ep_raw)

let start_service (type a) (t : Server.Server0.t)
    (spec : a inp Witness.service_spec) (f : a -> unit io) =
  let sv = register_service t spec in
  let rec loop () =
    let* var = accept t sv in
    ignore (f var);
    loop ()
  in
  loop ()

open Types
open Witness
open Monad.Common
open ServerImpl

type 'a ep = 'a Witness.ep = { ep_raw : Session.t; ep_witness : 'a Lin.t }
type 'm inp = 'm Witness.inp
type ('v, 'a) out = ('v, 'a) Witness.out
type 'x service_spec = 'x Witness.service_spec
type 'x service = { sv_service_id : service_id; sv_witness : 'x }

let register_service t spec =
  Server.register_service t ~spec:spec.sv_spec;
  { sv_service_id = spec.sv_spec.service_id; sv_witness = spec.sv_witness }

let parse_request : type a. a inp -> session -> request -> a io =
 fun (roles : _ inp) session request ->
  let role = request.request_pathspec.path_role in
  let (InpRole role) = List.assoc role roles in
  if role.path_spec.path <> request.request_pathspec.path then
    Monad.error_with
      (Format.asprintf
         "Path %a does not conform to the service %a at this point. Expected \
          next path: %a"
         Path.pp request.request_pathspec.path ServiceId.pp
         (session |> Session.service |> Service.id)
         Path.pp role.path_spec.path)
  else
    let* label = role.parse_label request.request_body in
    let (InpLabel label) = List.assoc label role.labels in
    let* payload = label.parse_payload request.request_body in
    return
    @@ role.role_constr.make_var
         (label.label_constr.make_var
            ( payload,
              {
                ep_raw = session;
                ep_witness =
                  Lin.create (Witness.witness (Lazy.force label.cont));
              } ))

let process_request session (roles : _ inp) request =
  let queue = Session.queues session request.request_pathspec.path_role in
  let* var =
    (fun () -> parse_request roles session request)
    |> Monad.handle_error ~handler:(fun err ->
           request.request_resolv (Error err);
           Monad.error err)
  in
  ConcurrentQueue.add_waiter queue.response_queue request.request_resolv;
  return var

let accept : type a. Server.t -> a inp service -> a io =
 fun t { sv_service_id; sv_witness = inp } ->
  let pathspecs = inp |> List.map (fun (_, InpRole role) -> role.path_spec) in
  let service = Server.service t sv_service_id in
  let* session, request = Service.wait_at_paths service pathspecs in
  process_request session inp request

let receive : 'a inp ep -> 'a io =
 fun ep ->
  let roles = Lin.get ep.ep_witness in
  let pathspecs = roles |> List.map (fun (_, InpRole role) -> role.path_spec) in
  let* request = Session.wait_at_paths ep.ep_raw pathspecs in
  process_request ep.ep_raw roles request

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

let msg_closing (session : session) =
  Monad.mpst_error
    (Format.asprintf "Session %a for service %a is closed" SessionId.pp
       (Session.id session) ServiceId.pp
       (Service.id (Session.service session)))

let close (ep : unit ep) =
  ignore @@ Lin.get ep.ep_witness;
  Session.kill ep.ep_raw (msg_closing ep.ep_raw)

let start_service (type a) (t : Server.t) (spec : a inp Witness.service_spec)
    (f : a -> unit io) =
  let sv = register_service t spec in
  let show_error err =
    Kxclib.Log0.error "error in service %a: %s" ServiceId.pp
      spec.sv_spec.service_id
      (Monad.error_to_string_full err)
  in
  let rec loop () =
    Monad.then_
      (fun () -> accept t sv)
      (function
        | Ok var ->
            (fun () -> f var)
            |> Monad.handle_error ~handler:(fun err ->
                   show_error err;
                   return ())
            |> ignore;
            loop ()
        | Error err ->
            show_error err;
            loop ())
  in
  ignore (loop ())

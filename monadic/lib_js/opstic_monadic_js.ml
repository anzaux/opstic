open! Kxclib
open! Opstic_js

type nonrec t = {
  server_ref : Server.t;
  protocol : Server.protocol_spec;
  conversation_id : conversation_id;
  self_role : role;
  role_http_session_id : (role, http_session_id option) Hashtbl.t;
}
[@@warning "-69"]

module ServerEndpoint :
  Opstic_monadic.Endpoint
    with type t = t
     and type 'x io = 'x ServerIo.t
     and type payload = payload = struct
  type 'x io = 'x ServerIo.t

  type nonrec t = t = {
    server_ref : Server.t;
    protocol : Server.protocol_spec;
    conversation_id : conversation_id;
    self_role : role;
    role_http_session_id : (role, http_session_id option) Hashtbl.t;
  }
  [@@warning "-69"]

  open ServerIo

  let ( let* ) = ServerIo.bind

  type nonrec payload = payload

  let get_session_id ~ctx t role =
    match Hashtbl.find t.role_http_session_id role with
    | Some s -> return s
    | None ->
        error_with
          (Format.asprintf "%s: Session is not established for role: %a" ctx
             Role.pp role)
    | exception Not_found ->
        error_with
          (Format.asprintf "%s: impossible: No such role: %a" ctx Role.pp role)

  let send t ~connection ~role ~label ~payload : unit io =
    assert (connection = Opstic.Connected);
    let role = Role.create role in
    let* http_session_id = get_session_id t role ~ctx:"send" in
    let msg : Json.jv = `obj [ ("label", `str label); ("payload", payload) ] in
    Server.Mpst.send_to_client t.server_ref ~http_session_id msg

  let receive t ~(connection : Opstic.connection) ~role =
    let role = Role.create role in
    let* payload =
      match connection with
      | Connected ->
          let* http_session_id = get_session_id t role ~ctx:"receive" in
          Server.Mpst.receive_from_client t.server_ref ~http_session_id
      | Join | JoinCorrelation ->
          let kind = if connection = Join then `Fresh else `Correlation in
          Server.Mpst.accept_join t.server_ref
            ~entrypoint_id:t.protocol.entrypoint_id ~role
            ~conversation_id:t.conversation_id ~kind
    in
    match payload |> (Jv.pump_field "payload" &> Jv.pump_field "label") with
    | `obj [ ("label", `str label); ("payload", payload) ] ->
        return (label, payload)
    | _ -> error_with "bad payload"

  let close _ =
    (* TODO purge all sessions from server *)
    ()
end

module Mpst_js = struct
  open ServerIo

  let ( let* ) = ServerIo.bind

  module Mpst_js = Opstic_monadic.Make (ServerIo) (ServerEndpoint)

  let accept_start_session :
      (* FIXME bad API style *)
      Server.t ->
      spec:Server.protocol_spec ->
      witness:'wit ->
      ('wit -> ([> ] as 'b) Mpst_js.Comm.inp) ->
      'b ServerIo.t =
   fun server ~spec ~witness call ->
    let* conversation_id, role, http_session_id =
      Server.Mpst.accept_initial server ~entrypoint_id:spec.entrypoint_id
        ~kind:`AsLeader ~roles:spec.other_roles
    in
    let role_http_session_id = Hashtbl.create 42 in
    spec.other_roles
    |> List.iter (fun role -> Hashtbl.replace role_http_session_id role None);
    Hashtbl.replace role_http_session_id role (Some http_session_id);
    let t =
      {
        server_ref = server;
        protocol = spec;
        conversation_id;
        self_role = spec.my_role;
        role_http_session_id;
      }
    in
    let inp = call witness in
    (* FIXME use Comm.receive instead *)
    let* payload = Server.Mpst.receive_from_client server ~http_session_id in
    let* payload =
      match Jv.access [ `f "message" ] payload with
      | Some x -> return x
      | _ ->
          error_with
            (Format.asprintf "no message body in request: %a" Json.pp_lit
               payload)
    in
    match payload |> (Jv.pump_field "payload" &> Jv.pump_field "label") with
    | `obj [ ("label", `str label); ("payload", v) ] ->
        let (Choice c) = Hashtbl.find inp.inp_choices label in
        let v = c.choice_marshal v in
        return
          (c.choice_variant.make_var (*fun x -> `lab x*)
             ( v,
               {
                 ep_raw = t;
                 ep_witness = Opstic_monadic.Lin.create c.choice_next_wit;
               } ))
    | _ -> error_with (Format.asprintf "bad payload: %a" Json.pp_lit payload)

  include Mpst_js
end
